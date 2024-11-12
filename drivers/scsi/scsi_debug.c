// SPDX-License-Identifier: GPL-2.0-or-later
/*
 * vvvvvvvvvvvvvvvvvvvvvvv Original vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
 *  Copyright (C) 1992  Eric Youngdale
 *  Simulate a host adapter with 2 disks attached.  Do a lot of checking
 *  to make sure that we are not getting blocks mixed up, and PANIC if
 *  anything out of the ordinary is seen.
 * ^^^^^^^^^^^^^^^^^^^^^^^ Original ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
 *
 * Copyright (C) 2001 - 2021 Douglas Gilbert
 *
 *  For documentation see http://sg.danny.cz/sg/scsi_debug.html
 */


#define pr_fmt(fmt) KBUILD_MODNAME ":%s: " fmt, __func__

#include <linux/module.h>
#include <linux/align.h>
#include <linux/kernel.h>
#include <linux/errno.h>
#include <linux/jiffies.h>
#include <linux/slab.h>
#include <linux/types.h>
#include <linux/string.h>
#include <linux/fs.h>
#include <linux/init.h>
#include <linux/proc_fs.h>
#include <linux/vmalloc.h>
#include <linux/moduleparam.h>
#include <linux/scatterlist.h>
#include <linux/blkdev.h>
#include <linux/crc-t10dif.h>
#include <linux/spinlock.h>
#include <linux/interrupt.h>
#include <linux/atomic.h>
#include <linux/hrtimer.h>
#include <linux/uuid.h>
#include <linux/t10-pi.h>
#include <linux/msdos_partition.h>
#include <linux/random.h>
#include <linux/xarray.h>
#include <linux/prefetch.h>
#include <linux/debugfs.h>
#include <linux/async.h>
#include <linux/cleanup.h>

#include <net/checksum.h>

#include <linux/unaligned.h>

#include <scsi/scsi.h>
#include <scsi/scsi_cmnd.h>
#include <scsi/scsi_device.h>
#include <scsi/scsi_host.h>
#include <scsi/scsicam.h>
#include <scsi/scsi_eh.h>
#include <scsi/scsi_tcq.h>
#include <scsi/scsi_dbg.h>

#include "sd.h"
#include "scsi_logging.h"

/* make sure inq_product_rev string corresponds to this version */
#define SDEBUG_VERSION "0191"	/* format to fit INQUIRY revision field */
static const char *sdebug_version_date = "20210520";

#define MY_NAME "scsi_debug"

/* Additional Sense Code (ASC) */
#define NO_ADDITIONAL_SENSE 0x0
#define OVERLAP_ATOMIC_COMMAND_ASC 0x0
#define OVERLAP_ATOMIC_COMMAND_ASCQ 0x23
#define LOGICAL_UNIT_NOT_READY 0x4
#define LOGICAL_UNIT_COMMUNICATION_FAILURE 0x8
#define UNRECOVERED_READ_ERR 0x11
#define PARAMETER_LIST_LENGTH_ERR 0x1a
#define INVALID_OPCODE 0x20
#define LBA_OUT_OF_RANGE 0x21
#define INVALID_FIELD_IN_CDB 0x24
#define INVALID_FIELD_IN_PARAM_LIST 0x26
#define WRITE_PROTECTED 0x27
#define UA_RESET_ASC 0x29
#define UA_CHANGED_ASC 0x2a
#define TARGET_CHANGED_ASC 0x3f
#define LUNS_CHANGED_ASCQ 0x0e
#define INSUFF_RES_ASC 0x55
#define INSUFF_RES_ASCQ 0x3
#define POWER_ON_RESET_ASCQ 0x0
#define POWER_ON_OCCURRED_ASCQ 0x1
#define BUS_RESET_ASCQ 0x2	/* scsi bus reset occurred */
#define MODE_CHANGED_ASCQ 0x1	/* mode parameters changed */
#define CAPACITY_CHANGED_ASCQ 0x9
#define SAVING_PARAMS_UNSUP 0x39
#define TRANSPORT_PROBLEM 0x4b
#define THRESHOLD_EXCEEDED 0x5d
#define LOW_POWER_COND_ON 0x5e
#define MISCOMPARE_VERIFY_ASC 0x1d
#define MICROCODE_CHANGED_ASCQ 0x1	/* with TARGET_CHANGED_ASC */
#define MICROCODE_CHANGED_WO_RESET_ASCQ 0x16
#define WRITE_ERROR_ASC 0xc
#define UNALIGNED_WRITE_ASCQ 0x4
#define WRITE_BOUNDARY_ASCQ 0x5
#define READ_INVDATA_ASCQ 0x6
#define READ_BOUNDARY_ASCQ 0x7
#define ATTEMPT_ACCESS_GAP 0x9
#define INSUFF_ZONE_ASCQ 0xe
/* see drivers/scsi/sense_codes.h */

/* Additional Sense Code Qualifier (ASCQ) */
#define ACK_NAK_TO 0x3

/* Default values for driver parameters */
#define DEF_NUM_HOST   1
#define DEF_NUM_TGTS   1
#define DEF_MAX_LUNS   1
/* With these defaults, this driver will make 1 host with 1 target
 * (id 0) containing 1 logical unit (lun 0). That is 1 device.
 */
#define DEF_ATO 1
#define DEF_CDB_LEN 10
#define DEF_JDELAY   1		/* if > 0 unit is a jiffy */
#define DEF_DEV_SIZE_PRE_INIT   0
#define DEF_DEV_SIZE_MB   8
#define DEF_ZBC_DEV_SIZE_MB   128
#define DEF_DIF 0
#define DEF_DIX 0
#define DEF_PER_HOST_STORE false
#define DEF_D_SENSE   0
#define DEF_EVERY_NTH   0
#define DEF_FAKE_RW	0
#define DEF_GUARD 0
#define DEF_HOST_LOCK 0
#define DEF_LBPU 0
#define DEF_LBPWS 0
#define DEF_LBPWS10 0
#define DEF_LBPRZ 1
#define DEF_LOWEST_ALIGNED 0
#define DEF_NDELAY   0		/* if > 0 unit is a nanosecond */
#define DEF_NO_LUN_0   0
#define DEF_NUM_PARTS   0
#define DEF_OPTS   0
#define DEF_OPT_BLKS 1024
#define DEF_PHYSBLK_EXP 0
#define DEF_OPT_XFERLEN_EXP 0
#define DEF_PTYPE   TYPE_DISK
#define DEF_RANDOM false
#define DEF_REMOVABLE false
#define DEF_SCSI_LEVEL   7    /* INQUIRY, byte2 [6->SPC-4; 7->SPC-5] */
#define DEF_SECTOR_SIZE 512
#define DEF_UNMAP_ALIGNMENT 0
#define DEF_UNMAP_GRANULARITY 1
#define DEF_UNMAP_MAX_BLOCKS 0xFFFFFFFF
#define DEF_UNMAP_MAX_DESC 256
#define DEF_VIRTUAL_GB   0
#define DEF_VPD_USE_HOSTNO 1
#define DEF_WRITESAME_LENGTH 0xFFFF
#define DEF_ATOMIC_WR 0
#define DEF_ATOMIC_WR_MAX_LENGTH 8192
#define DEF_ATOMIC_WR_ALIGN 2
#define DEF_ATOMIC_WR_GRAN 2
#define DEF_ATOMIC_WR_MAX_LENGTH_BNDRY (DEF_ATOMIC_WR_MAX_LENGTH)
#define DEF_ATOMIC_WR_MAX_BNDRY 128
#define DEF_STRICT 0
#define DEF_STATISTICS false
#define DEF_SUBMIT_QUEUES 1
#define DEF_TUR_MS_TO_READY 0
#define DEF_UUID_CTL 0
#define JDELAY_OVERRIDDEN -9999

/* Default parameters for ZBC drives */
#define DEF_ZBC_ZONE_SIZE_MB	128
#define DEF_ZBC_MAX_OPEN_ZONES	8
#define DEF_ZBC_NR_CONV_ZONES	1

#define SDEBUG_LUN_0_VAL 0

/* bit mask values for sdebug_opts */
#define SDEBUG_OPT_NOISE		1
#define SDEBUG_OPT_MEDIUM_ERR		2
#define SDEBUG_OPT_TIMEOUT		4
#define SDEBUG_OPT_RECOVERED_ERR	8
#define SDEBUG_OPT_TRANSPORT_ERR	16
#define SDEBUG_OPT_DIF_ERR		32
#define SDEBUG_OPT_DIX_ERR		64
#define SDEBUG_OPT_MAC_TIMEOUT		128
#define SDEBUG_OPT_SHORT_TRANSFER	0x100
#define SDEBUG_OPT_Q_NOISE		0x200
#define SDEBUG_OPT_ALL_TSF		0x400	/* ignore */
#define SDEBUG_OPT_RARE_TSF		0x800
#define SDEBUG_OPT_N_WCE		0x1000
#define SDEBUG_OPT_RESET_NOISE		0x2000
#define SDEBUG_OPT_NO_CDB_NOISE		0x4000
#define SDEBUG_OPT_HOST_BUSY		0x8000
#define SDEBUG_OPT_CMD_ABORT		0x10000
#define SDEBUG_OPT_ALL_NOISE (SDEBUG_OPT_NOISE | SDEBUG_OPT_Q_NOISE | \
			      SDEBUG_OPT_RESET_NOISE)
#define SDEBUG_OPT_ALL_INJECTING (SDEBUG_OPT_RECOVERED_ERR | \
				  SDEBUG_OPT_TRANSPORT_ERR | \
				  SDEBUG_OPT_DIF_ERR | SDEBUG_OPT_DIX_ERR | \
				  SDEBUG_OPT_SHORT_TRANSFER | \
				  SDEBUG_OPT_HOST_BUSY | \
				  SDEBUG_OPT_CMD_ABORT)
#define SDEBUG_OPT_RECOV_DIF_DIX (SDEBUG_OPT_RECOVERED_ERR | \
				  SDEBUG_OPT_DIF_ERR | SDEBUG_OPT_DIX_ERR)

/* As indicated in SAM-5 and SPC-4 Unit Attentions (UAs) are returned in
 * priority order. In the subset implemented here lower numbers have higher
 * priority. The UA numbers should be a sequence starting from 0 with
 * SDEBUG_NUM_UAS being 1 higher than the highest numbered UA. */
#define SDEBUG_UA_POR 0		/* Power on, reset, or bus device reset */
#define SDEBUG_UA_POOCCUR 1	/* Power on occurred */
#define SDEBUG_UA_BUS_RESET 2
#define SDEBUG_UA_MODE_CHANGED 3
#define SDEBUG_UA_CAPACITY_CHANGED 4
#define SDEBUG_UA_LUNS_CHANGED 5
#define SDEBUG_UA_MICROCODE_CHANGED 6	/* simulate firmware change */
#define SDEBUG_UA_MICROCODE_CHANGED_WO_RESET 7
#define SDEBUG_NUM_UAS 8

/* when 1==SDEBUG_OPT_MEDIUM_ERR, a medium error is simulated at this
 * sector on read commands: */
#define OPT_MEDIUM_ERR_ADDR   0x1234 /* that's sector 4660 in decimal */
#define OPT_MEDIUM_ERR_NUM    10     /* number of consecutive medium errs */

/* SDEBUG_CANQUEUE is the maximum number of commands that can be queued
 * (for response) per submit queue at one time. Can be reduced by max_queue
 * option. Command responses are not queued when jdelay=0 and ndelay=0. The
 * per-device DEF_CMD_PER_LUN can be changed via sysfs:
 * /sys/class/scsi_device/<h:c:t:l>/device/queue_depth
 * but cannot exceed SDEBUG_CANQUEUE .
 */
#define SDEBUG_CANQUEUE_WORDS  3	/* a WORD is bits in a long */
#define SDEBUG_CANQUEUE  (SDEBUG_CANQUEUE_WORDS * BITS_PER_LONG)
#define DEF_CMD_PER_LUN  SDEBUG_CANQUEUE

/* UA - Unit Attention; SA - Service Action; SSU - Start Stop Unit */
#define F_D_IN			1	/* Data-in command (e.g. READ) */
#define F_D_OUT			2	/* Data-out command (e.g. WRITE) */
#define F_D_OUT_MAYBE		4	/* WRITE SAME, NDOB bit */
#define F_D_UNKN		8
#define F_RL_WLUN_OK		0x10	/* allowed with REPORT LUNS W-LUN */
#define F_SKIP_UA		0x20	/* bypass UAs (e.g. INQUIRY command) */
#define F_DELAY_OVERR		0x40	/* for commands like INQUIRY */
#define F_SA_LOW		0x80	/* SA is in cdb byte 1, bits 4 to 0 */
#define F_SA_HIGH		0x100	/* SA is in cdb bytes 8 and 9 */
#define F_INV_OP		0x200	/* invalid opcode (not supported) */
#define F_FAKE_RW		0x400	/* bypass resp_*() when fake_rw set */
#define F_M_ACCESS		0x800	/* media access, reacts to SSU state */
#define F_SSU_DELAY		0x1000	/* SSU command delay (long-ish) */
#define F_SYNC_DELAY		0x2000	/* SYNCHRONIZE CACHE delay */

/* Useful combinations of the above flags */
#define FF_RESPOND (F_RL_WLUN_OK | F_SKIP_UA | F_DELAY_OVERR)
#define FF_MEDIA_IO (F_M_ACCESS | F_FAKE_RW)
#define FF_SA (F_SA_HIGH | F_SA_LOW)
#define F_LONG_DELAY		(F_SSU_DELAY | F_SYNC_DELAY)

#define SDEBUG_MAX_PARTS 4

#define SDEBUG_MAX_CMD_LEN 32

#define SDEB_XA_NOT_IN_USE XA_MARK_1

static struct kmem_cache *queued_cmd_cache;

#define TO_QUEUED_CMD(scmd)  ((void *)(scmd)->host_scribble)
#define ASSIGN_QUEUED_CMD(scmnd, qc) { (scmnd)->host_scribble = (void *) qc; }

/* Zone types (zbcr05 table 25) */
enum sdebug_z_type {
	ZBC_ZTYPE_CNV	= 0x1,
	ZBC_ZTYPE_SWR	= 0x2,
	ZBC_ZTYPE_SWP	= 0x3,
	/* ZBC_ZTYPE_SOBR = 0x4, */
	ZBC_ZTYPE_GAP	= 0x5,
};

/* enumeration names taken from table 26, zbcr05 */
enum sdebug_z_cond {
	ZBC_NOT_WRITE_POINTER	= 0x0,
	ZC1_EMPTY		= 0x1,
	ZC2_IMPLICIT_OPEN	= 0x2,
	ZC3_EXPLICIT_OPEN	= 0x3,
	ZC4_CLOSED		= 0x4,
	ZC6_READ_ONLY		= 0xd,
	ZC5_FULL		= 0xe,
	ZC7_OFFLINE		= 0xf,
};

struct sdeb_zone_state {	/* ZBC: per zone state */
	enum sdebug_z_type z_type;
	enum sdebug_z_cond z_cond;
	bool z_non_seq_resource;
	unsigned int z_size;
	sector_t z_start;
	sector_t z_wp;
};

enum sdebug_err_type {
	ERR_TMOUT_CMD		= 0,	/* make specific scsi command timeout */
	ERR_FAIL_QUEUE_CMD	= 1,	/* make specific scsi command's */
					/* queuecmd return failed */
	ERR_FAIL_CMD		= 2,	/* make specific scsi command's */
					/* queuecmd return succeed but */
					/* with errors set in scsi_cmnd */
	ERR_ABORT_CMD_FAILED	= 3,	/* control return FAILED from */
					/* scsi_debug_abort() */
	ERR_LUN_RESET_FAILED	= 4,	/* control return FAILED from */
					/* scsi_debug_device_reseLUN_RESET_FAILEDt() */
};

struct sdebug_err_inject {
	int type;
	struct list_head list;
	int cnt;
	unsigned char cmd;
	struct rcu_head rcu;

	union {
		/*
		 * For ERR_FAIL_QUEUE_CMD
		 */
		int queuecmd_ret;

		/*
		 * For ERR_FAIL_CMD
		 */
		struct {
			unsigned char host_byte;
			unsigned char driver_byte;
			unsigned char status_byte;
			unsigned char sense_key;
			unsigned char asc;
			unsigned char asq;
		};
	};
};

struct sdebug_dev_info {
	struct list_head dev_list;
	unsigned int channel;
	unsigned int target;
	u64 lun;
	uuid_t lu_name;
	struct sdebug_host_info *sdbg_host;
	unsigned long uas_bm[1];
	atomic_t stopped;	/* 1: by SSU, 2: device start */
	bool used;

	/* For ZBC devices */
	bool zoned;
	unsigned int zcap;
	unsigned int zsize;
	unsigned int zsize_shift;
	unsigned int nr_zones;
	unsigned int nr_conv_zones;
	unsigned int nr_seq_zones;
	unsigned int nr_imp_open;
	unsigned int nr_exp_open;
	unsigned int nr_closed;
	unsigned int max_open;
	ktime_t create_ts;	/* time since bootup that this device was created */
	struct sdeb_zone_state *zstate;

	struct dentry *debugfs_entry;
	struct spinlock list_lock;
	struct list_head inject_err_list;
};

struct sdebug_target_info {
	bool reset_fail;
	struct dentry *debugfs_entry;
};

struct sdebug_host_info {
	struct list_head host_list;
	int si_idx;	/* sdeb_store_info (per host) xarray index */
	struct Scsi_Host *shost;
	struct device dev;
	struct list_head dev_info_list;
};

/* There is an xarray of pointers to this struct's objects, one per host */
struct sdeb_store_info {
	rwlock_t macc_data_lck;	/* for media data access on this store */
	rwlock_t macc_meta_lck;	/* for atomic media meta access on this store */
	rwlock_t macc_sector_lck;	/* per-sector media data access on this store */
	u8 *storep;		/* user data storage (ram) */
	struct t10_pi_tuple *dif_storep; /* protection info */
	void *map_storep;	/* provisioning map */
};

#define dev_to_sdebug_host(d)	\
	container_of(d, struct sdebug_host_info, dev)

#define shost_to_sdebug_host(shost)	\
	dev_to_sdebug_host(shost->dma_dev)

enum sdeb_defer_type {SDEB_DEFER_NONE = 0, SDEB_DEFER_HRT = 1,
		      SDEB_DEFER_WQ = 2, SDEB_DEFER_POLL = 3};

struct sdebug_defer {
	struct hrtimer hrt;
	struct execute_work ew;
	ktime_t cmpl_ts;/* time since boot to complete this cmd */
	int issuing_cpu;
	bool aborted;	/* true when blk_abort_request() already called */
	enum sdeb_defer_type defer_t;
};

struct sdebug_device_access_info {
	bool atomic_write;
	u64 lba;
	u32 num;
	struct scsi_cmnd *self;
};

struct sdebug_queued_cmd {
	/* corresponding bit set in in_use_bm[] in owning struct sdebug_queue
	 * instance indicates this slot is in use.
	 */
	struct sdebug_defer sd_dp;
	struct scsi_cmnd *scmd;
	struct sdebug_device_access_info *i;
};

struct sdebug_scsi_cmd {
	spinlock_t   lock;
};

static atomic_t sdebug_cmnd_count;   /* number of incoming commands */
static atomic_t sdebug_completions;  /* count of deferred completions */
static atomic_t sdebug_miss_cpus;    /* submission + completion cpus differ */
static atomic_t sdebug_a_tsf;	     /* 'almost task set full' counter */
static atomic_t sdeb_inject_pending;
static atomic_t sdeb_mq_poll_count;  /* bumped when mq_poll returns > 0 */

struct opcode_info_t {
	u8 num_attached;	/* 0 if this is it (i.e. a leaf); use 0xff */
				/* for terminating element */
	u8 opcode;		/* if num_attached > 0, preferred */
	u16 sa;			/* service action */
	u32 flags;		/* OR-ed set of SDEB_F_* */
	int (*pfp)(struct scsi_cmnd *, struct sdebug_dev_info *);
	const struct opcode_info_t *arrp;  /* num_attached elements or NULL */
	u8 len_mask[16];	/* len_mask[0]-->cdb_len, then mask for cdb */
				/* 1 to min(cdb_len, 15); ignore cdb[15...] */
};

/* SCSI opcodes (first byte of cdb) of interest mapped onto these indexes */
enum sdeb_opcode_index {
	SDEB_I_INVALID_OPCODE =	0,
	SDEB_I_INQUIRY = 1,
	SDEB_I_REPORT_LUNS = 2,
	SDEB_I_REQUEST_SENSE = 3,
	SDEB_I_TEST_UNIT_READY = 4,
	SDEB_I_MODE_SENSE = 5,		/* 6, 10 */
	SDEB_I_MODE_SELECT = 6,		/* 6, 10 */
	SDEB_I_LOG_SENSE = 7,
	SDEB_I_READ_CAPACITY = 8,	/* 10; 16 is in SA_IN(16) */
	SDEB_I_READ = 9,		/* 6, 10, 12, 16 */
	SDEB_I_WRITE = 10,		/* 6, 10, 12, 16 */
	SDEB_I_START_STOP = 11,
	SDEB_I_SERV_ACT_IN_16 = 12,	/* add ...SERV_ACT_IN_12 if needed */
	SDEB_I_SERV_ACT_OUT_16 = 13,	/* add ...SERV_ACT_OUT_12 if needed */
	SDEB_I_MAINT_IN = 14,
	SDEB_I_MAINT_OUT = 15,
	SDEB_I_VERIFY = 16,		/* VERIFY(10), VERIFY(16) */
	SDEB_I_VARIABLE_LEN = 17,	/* READ(32), WRITE(32), WR_SCAT(32) */
	SDEB_I_RESERVE = 18,		/* 6, 10 */
	SDEB_I_RELEASE = 19,		/* 6, 10 */
	SDEB_I_ALLOW_REMOVAL = 20,	/* PREVENT ALLOW MEDIUM REMOVAL */
	SDEB_I_REZERO_UNIT = 21,	/* REWIND in SSC */
	SDEB_I_ATA_PT = 22,		/* 12, 16 */
	SDEB_I_SEND_DIAG = 23,
	SDEB_I_UNMAP = 24,
	SDEB_I_WRITE_BUFFER = 25,
	SDEB_I_WRITE_SAME = 26,		/* 10, 16 */
	SDEB_I_SYNC_CACHE = 27,		/* 10, 16 */
	SDEB_I_COMP_WRITE = 28,
	SDEB_I_PRE_FETCH = 29,		/* 10, 16 */
	SDEB_I_ZONE_OUT = 30,		/* 0x94+SA; includes no data xfer */
	SDEB_I_ZONE_IN = 31,		/* 0x95+SA; all have data-in */
	SDEB_I_ATOMIC_WRITE_16 = 32,
	SDEB_I_LAST_ELEM_P1 = 33,	/* keep this last (previous + 1) */
};


static const unsigned char opcode_ind_arr[256] = {
/* 0x0; 0x0->0x1f: 6 byte cdbs */
	SDEB_I_TEST_UNIT_READY, SDEB_I_REZERO_UNIT, 0, SDEB_I_REQUEST_SENSE,
	    0, 0, 0, 0,
	SDEB_I_READ, 0, SDEB_I_WRITE, 0, 0, 0, 0, 0,
	0, 0, SDEB_I_INQUIRY, 0, 0, SDEB_I_MODE_SELECT, SDEB_I_RESERVE,
	    SDEB_I_RELEASE,
	0, 0, SDEB_I_MODE_SENSE, SDEB_I_START_STOP, 0, SDEB_I_SEND_DIAG,
	    SDEB_I_ALLOW_REMOVAL, 0,
/* 0x20; 0x20->0x3f: 10 byte cdbs */
	0, 0, 0, 0, 0, SDEB_I_READ_CAPACITY, 0, 0,
	SDEB_I_READ, 0, SDEB_I_WRITE, 0, 0, 0, 0, SDEB_I_VERIFY,
	0, 0, 0, 0, SDEB_I_PRE_FETCH, SDEB_I_SYNC_CACHE, 0, 0,
	0, 0, 0, SDEB_I_WRITE_BUFFER, 0, 0, 0, 0,
/* 0x40; 0x40->0x5f: 10 byte cdbs */
	0, SDEB_I_WRITE_SAME, SDEB_I_UNMAP, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, SDEB_I_LOG_SENSE, 0, 0,
	0, 0, 0, 0, 0, SDEB_I_MODE_SELECT, SDEB_I_RESERVE,
	    SDEB_I_RELEASE,
	0, 0, SDEB_I_MODE_SENSE, 0, 0, 0, 0, 0,
/* 0x60; 0x60->0x7d are reserved, 0x7e is "extended cdb" */
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	0, SDEB_I_VARIABLE_LEN,
/* 0x80; 0x80->0x9f: 16 byte cdbs */
	0, 0, 0, 0, 0, SDEB_I_ATA_PT, 0, 0,
	SDEB_I_READ, SDEB_I_COMP_WRITE, SDEB_I_WRITE, 0,
	0, 0, 0, SDEB_I_VERIFY,
	SDEB_I_PRE_FETCH, SDEB_I_SYNC_CACHE, 0, SDEB_I_WRITE_SAME,
	SDEB_I_ZONE_OUT, SDEB_I_ZONE_IN, 0, 0,
	0, 0, 0, 0,
	SDEB_I_ATOMIC_WRITE_16, 0, SDEB_I_SERV_ACT_IN_16, SDEB_I_SERV_ACT_OUT_16,
/* 0xa0; 0xa0->0xbf: 12 byte cdbs */
	SDEB_I_REPORT_LUNS, SDEB_I_ATA_PT, 0, SDEB_I_MAINT_IN,
	     SDEB_I_MAINT_OUT, 0, 0, 0,
	SDEB_I_READ, 0 /* SDEB_I_SERV_ACT_OUT_12 */, SDEB_I_WRITE,
	     0 /* SDEB_I_SERV_ACT_IN_12 */, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0,
/* 0xc0; 0xc0->0xff: vendor specific */
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
};

/*
 * The following "response" functions return the SCSI mid-level's 4 byte
 * tuple-in-an-int. To handle commands with an IMMED bit, for a faster
 * command completion, they can mask their return value with
 * SDEG_RES_IMMED_MASK .
 */
#define SDEG_RES_IMMED_MASK 0x40000000

static int resp_inquiry(struct scsi_cmnd *, struct sdebug_dev_info *);
static int resp_report_luns(struct scsi_cmnd *, struct sdebug_dev_info *);
static int resp_requests(struct scsi_cmnd *, struct sdebug_dev_info *);
static int resp_mode_sense(struct scsi_cmnd *, struct sdebug_dev_info *);
static int resp_mode_select(struct scsi_cmnd *, struct sdebug_dev_info *);
static int resp_log_sense(struct scsi_cmnd *, struct sdebug_dev_info *);
static int resp_readcap(struct scsi_cmnd *, struct sdebug_dev_info *);
static int resp_read_dt0(struct scsi_cmnd *, struct sdebug_dev_info *);
static int resp_write_dt0(struct scsi_cmnd *, struct sdebug_dev_info *);
static int resp_write_scat(struct scsi_cmnd *, struct sdebug_dev_info *);
static int resp_start_stop(struct scsi_cmnd *, struct sdebug_dev_info *);
static int resp_readcap16(struct scsi_cmnd *, struct sdebug_dev_info *);
static int resp_get_lba_status(struct scsi_cmnd *, struct sdebug_dev_info *);
static int resp_get_stream_status(struct scsi_cmnd *scp,
				  struct sdebug_dev_info *devip);
static int resp_report_tgtpgs(struct scsi_cmnd *, struct sdebug_dev_info *);
static int resp_unmap(struct scsi_cmnd *, struct sdebug_dev_info *);
static int resp_rsup_opcodes(struct scsi_cmnd *, struct sdebug_dev_info *);
static int resp_rsup_tmfs(struct scsi_cmnd *, struct sdebug_dev_info *);
static int resp_verify(struct scsi_cmnd *, struct sdebug_dev_info *);
static int resp_write_same_10(struct scsi_cmnd *, struct sdebug_dev_info *);
static int resp_write_same_16(struct scsi_cmnd *, struct sdebug_dev_info *);
static int resp_comp_write(struct scsi_cmnd *, struct sdebug_dev_info *);
static int resp_write_buffer(struct scsi_cmnd *, struct sdebug_dev_info *);
static int resp_sync_cache(struct scsi_cmnd *, struct sdebug_dev_info *);
static int resp_pre_fetch(struct scsi_cmnd *, struct sdebug_dev_info *);
static int resp_report_zones(struct scsi_cmnd *, struct sdebug_dev_info *);
static int resp_atomic_write(struct scsi_cmnd *, struct sdebug_dev_info *);
static int resp_open_zone(struct scsi_cmnd *, struct sdebug_dev_info *);
static int resp_close_zone(struct scsi_cmnd *, struct sdebug_dev_info *);
static int resp_finish_zone(struct scsi_cmnd *, struct sdebug_dev_info *);
static int resp_rwp_zone(struct scsi_cmnd *, struct sdebug_dev_info *);

static int sdebug_do_add_host(bool mk_new_store);
static int sdebug_add_host_helper(int per_host_idx);
static void sdebug_do_remove_host(bool the_end);
static int sdebug_add_store(void);
static void sdebug_erase_store(int idx, struct sdeb_store_info *sip);
static void sdebug_erase_all_stores(bool apart_from_first);

static void sdebug_free_queued_cmd(struct sdebug_queued_cmd *sqcp);

/*
 * The following are overflow arrays for cdbs that "hit" the same index in
 * the opcode_info_arr array. The most time sensitive (or commonly used) cdb
 * should be placed in opcode_info_arr[], the others should be placed here.
 */
static const struct opcode_info_t msense_iarr[] = {
	{0, 0x1a, 0, F_D_IN, NULL, NULL,
	    {6,  0xe8, 0xff, 0xff, 0xff, 0xc7, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0} },
};

static const struct opcode_info_t mselect_iarr[] = {
	{0, 0x15, 0, F_D_OUT, NULL, NULL,
	    {6,  0xf1, 0, 0, 0xff, 0xc7, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0} },
};

static const struct opcode_info_t read_iarr[] = {
	{0, 0x28, 0, F_D_IN | FF_MEDIA_IO, resp_read_dt0, NULL,/* READ(10) */
	    {10,  0xff, 0xff, 0xff, 0xff, 0xff, 0x3f, 0xff, 0xff, 0xc7, 0, 0,
	     0, 0, 0, 0} },
	{0, 0x8, 0, F_D_IN | FF_MEDIA_IO, resp_read_dt0, NULL, /* READ(6) */
	    {6,  0xff, 0xff, 0xff, 0xff, 0xc7, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0} },
	{0, 0xa8, 0, F_D_IN | FF_MEDIA_IO, resp_read_dt0, NULL,/* READ(12) */
	    {12,  0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xbf,
	     0xc7, 0, 0, 0, 0} },
};

static const struct opcode_info_t write_iarr[] = {
	{0, 0x2a, 0, F_D_OUT | FF_MEDIA_IO, resp_write_dt0,  /* WRITE(10) */
	    NULL, {10,  0xfb, 0xff, 0xff, 0xff, 0xff, 0x3f, 0xff, 0xff, 0xc7,
		   0, 0, 0, 0, 0, 0} },
	{0, 0xa, 0, F_D_OUT | FF_MEDIA_IO, resp_write_dt0,   /* WRITE(6) */
	    NULL, {6,  0xff, 0xff, 0xff, 0xff, 0xc7, 0, 0, 0, 0, 0, 0, 0,
		   0, 0, 0} },
	{0, 0xaa, 0, F_D_OUT | FF_MEDIA_IO, resp_write_dt0,  /* WRITE(12) */
	    NULL, {12,  0xfb, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
		   0xbf, 0xc7, 0, 0, 0, 0} },
};

static const struct opcode_info_t verify_iarr[] = {
	{0, 0x2f, 0, F_D_OUT_MAYBE | FF_MEDIA_IO, resp_verify,/* VERIFY(10) */
	    NULL, {10,  0xf7, 0xff, 0xff, 0xff, 0xff, 0xbf, 0xff, 0xff, 0xc7,
		   0, 0, 0, 0, 0, 0} },
};

static const struct opcode_info_t sa_in_16_iarr[] = {
	{0, 0x9e, 0x12, F_SA_LOW | F_D_IN, resp_get_lba_status, NULL,
	    {16,  0x12, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
	     0xff, 0xff, 0xff, 0, 0xc7} },	/* GET LBA STATUS(16) */
	{0, 0x9e, 0x16, F_SA_LOW | F_D_IN, resp_get_stream_status, NULL,
	    {16, 0x16, 0, 0, 0xff, 0xff, 0, 0, 0, 0, 0xff, 0xff, 0xff, 0xff,
	     0, 0} },	/* GET STREAM STATUS */
};

static const struct opcode_info_t vl_iarr[] = {	/* VARIABLE LENGTH */
	{0, 0x7f, 0xb, F_SA_HIGH | F_D_OUT | FF_MEDIA_IO, resp_write_dt0,
	    NULL, {32,  0xc7, 0, 0, 0, 0, 0x3f, 0x18, 0x0, 0xb, 0xfa,
		   0, 0xff, 0xff, 0xff, 0xff} },	/* WRITE(32) */
	{0, 0x7f, 0x11, F_SA_HIGH | F_D_OUT | FF_MEDIA_IO, resp_write_scat,
	    NULL, {32,  0xc7, 0, 0, 0, 0, 0x3f, 0x18, 0x0, 0x11, 0xf8,
		   0, 0xff, 0xff, 0x0, 0x0} },	/* WRITE SCATTERED(32) */
};

static const struct opcode_info_t maint_in_iarr[] = {	/* MAINT IN */
	{0, 0xa3, 0xc, F_SA_LOW | F_D_IN, resp_rsup_opcodes, NULL,
	    {12,  0xc, 0x87, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0,
	     0xc7, 0, 0, 0, 0} }, /* REPORT SUPPORTED OPERATION CODES */
	{0, 0xa3, 0xd, F_SA_LOW | F_D_IN, resp_rsup_tmfs, NULL,
	    {12,  0xd, 0x80, 0, 0, 0, 0xff, 0xff, 0xff, 0xff, 0, 0xc7, 0, 0,
	     0, 0} },	/* REPORTED SUPPORTED TASK MANAGEMENT FUNCTIONS */
};

static const struct opcode_info_t write_same_iarr[] = {
	{0, 0x93, 0, F_D_OUT_MAYBE | FF_MEDIA_IO, resp_write_same_16, NULL,
	    {16,  0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
	     0xff, 0xff, 0xff, 0x3f, 0xc7} },		/* WRITE SAME(16) */
};

static const struct opcode_info_t reserve_iarr[] = {
	{0, 0x16, 0, F_D_OUT, NULL, NULL,		/* RESERVE(6) */
	    {6,  0x1f, 0xff, 0xff, 0xff, 0xc7, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0} },
};

static const struct opcode_info_t release_iarr[] = {
	{0, 0x17, 0, F_D_OUT, NULL, NULL,		/* RELEASE(6) */
	    {6,  0x1f, 0xff, 0, 0, 0xc7, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0} },
};

static const struct opcode_info_t sync_cache_iarr[] = {
	{0, 0x91, 0, F_SYNC_DELAY | F_M_ACCESS, resp_sync_cache, NULL,
	    {16,  0x6, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
	     0xff, 0xff, 0xff, 0xff, 0x3f, 0xc7} },	/* SYNC_CACHE (16) */
};

static const struct opcode_info_t pre_fetch_iarr[] = {
	{0, 0x90, 0, F_SYNC_DELAY | FF_MEDIA_IO, resp_pre_fetch, NULL,
	    {16,  0x2, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
	     0xff, 0xff, 0xff, 0xff, 0x3f, 0xc7} },	/* PRE-FETCH (16) */
};

static const struct opcode_info_t zone_out_iarr[] = {	/* ZONE OUT(16) */
	{0, 0x94, 0x1, F_SA_LOW | F_M_ACCESS, resp_close_zone, NULL,
	    {16, 0x1, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
	     0xff, 0, 0, 0xff, 0xff, 0x1, 0xc7} },	/* CLOSE ZONE */
	{0, 0x94, 0x2, F_SA_LOW | F_M_ACCESS, resp_finish_zone, NULL,
	    {16, 0x2, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
	     0xff, 0, 0, 0xff, 0xff, 0x1, 0xc7} },	/* FINISH ZONE */
	{0, 0x94, 0x4, F_SA_LOW | F_M_ACCESS, resp_rwp_zone, NULL,
	    {16, 0x4, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
	     0xff, 0, 0, 0xff, 0xff, 0x1, 0xc7} },  /* RESET WRITE POINTER */
};

static const struct opcode_info_t zone_in_iarr[] = {	/* ZONE IN(16) */
	{0, 0x95, 0x6, F_SA_LOW | F_D_IN | F_M_ACCESS, NULL, NULL,
	    {16, 0x6, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
	     0xff, 0xff, 0xff, 0xff, 0x3f, 0xc7} }, /* REPORT ZONES */
};


/* This array is accessed via SDEB_I_* values. Make sure all are mapped,
 * plus the terminating elements for logic that scans this table such as
 * REPORT SUPPORTED OPERATION CODES. */
static const struct opcode_info_t opcode_info_arr[SDEB_I_LAST_ELEM_P1 + 1] = {
/* 0 */
	{0, 0, 0, F_INV_OP | FF_RESPOND, NULL, NULL,	/* unknown opcodes */
	    {0,  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0} },
	{0, 0x12, 0, FF_RESPOND | F_D_IN, resp_inquiry, NULL, /* INQUIRY */
	    {6,  0xe3, 0xff, 0xff, 0xff, 0xc7, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0} },
	{0, 0xa0, 0, FF_RESPOND | F_D_IN, resp_report_luns, NULL,
	    {12,  0xe3, 0xff, 0, 0, 0, 0xff, 0xff, 0xff, 0xff, 0, 0xc7, 0, 0,
	     0, 0} },					/* REPORT LUNS */
	{0, 0x3, 0, FF_RESPOND | F_D_IN, resp_requests, NULL,
	    {6,  0xe1, 0, 0, 0xff, 0xc7, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0} },
	{0, 0x0, 0, F_M_ACCESS | F_RL_WLUN_OK, NULL, NULL,/* TEST UNIT READY */
	    {6,  0, 0, 0, 0, 0xc7, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0} },
/* 5 */
	{ARRAY_SIZE(msense_iarr), 0x5a, 0, F_D_IN,	/* MODE SENSE(10) */
	    resp_mode_sense, msense_iarr, {10,  0xf8, 0xff, 0xff, 0, 0, 0,
		0xff, 0xff, 0xc7, 0, 0, 0, 0, 0, 0} },
	{ARRAY_SIZE(mselect_iarr), 0x55, 0, F_D_OUT,	/* MODE SELECT(10) */
	    resp_mode_select, mselect_iarr, {10,  0xf1, 0, 0, 0, 0, 0, 0xff,
		0xff, 0xc7, 0, 0, 0, 0, 0, 0} },
	{0, 0x4d, 0, F_D_IN, resp_log_sense, NULL,	/* LOG SENSE */
	    {10,  0xe3, 0xff, 0xff, 0, 0xff, 0xff, 0xff, 0xff, 0xc7, 0, 0, 0,
	     0, 0, 0} },
	{0, 0x25, 0, F_D_IN, resp_readcap, NULL,    /* READ CAPACITY(10) */
	    {10,  0xe1, 0xff, 0xff, 0xff, 0xff, 0, 0, 0x1, 0xc7, 0, 0, 0, 0,
	     0, 0} },
	{ARRAY_SIZE(read_iarr), 0x88, 0, F_D_IN | FF_MEDIA_IO, /* READ(16) */
	    resp_read_dt0, read_iarr, {16,  0xfe, 0xff, 0xff, 0xff, 0xff,
	    0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xc7} },
/* 10 */
	{ARRAY_SIZE(write_iarr), 0x8a, 0, F_D_OUT | FF_MEDIA_IO,
	    resp_write_dt0, write_iarr,			/* WRITE(16) */
		{16,  0xfa, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
		 0xff, 0xff, 0xff, 0xff, 0xff, 0xc7} },
	{0, 0x1b, 0, F_SSU_DELAY, resp_start_stop, NULL,/* START STOP UNIT */
	    {6,  0x1, 0, 0xf, 0xf7, 0xc7, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0} },
	{ARRAY_SIZE(sa_in_16_iarr), 0x9e, 0x10, F_SA_LOW | F_D_IN,
	    resp_readcap16, sa_in_16_iarr, /* SA_IN(16), READ CAPACITY(16) */
		{16,  0x10, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
		 0xff, 0xff, 0xff, 0xff, 0x1, 0xc7} },
	{0, 0x9f, 0x12, F_SA_LOW | F_D_OUT | FF_MEDIA_IO, resp_write_scat,
	    NULL, {16,  0x12, 0xf9, 0x0, 0xff, 0xff, 0, 0, 0xff, 0xff, 0xff,
	    0xff, 0xff, 0xff, 0xff, 0xc7} },  /* SA_OUT(16), WRITE SCAT(16) */
	{ARRAY_SIZE(maint_in_iarr), 0xa3, 0xa, F_SA_LOW | F_D_IN,
	    resp_report_tgtpgs,	/* MAINT IN, REPORT TARGET PORT GROUPS */
		maint_in_iarr, {12,  0xea, 0, 0, 0, 0, 0xff, 0xff, 0xff,
				0xff, 0, 0xc7, 0, 0, 0, 0} },
/* 15 */
	{0, 0, 0, F_INV_OP | FF_RESPOND, NULL, NULL, /* MAINT OUT */
	    {0,  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0} },
	{ARRAY_SIZE(verify_iarr), 0x8f, 0,
	    F_D_OUT_MAYBE | FF_MEDIA_IO, resp_verify,	/* VERIFY(16) */
	    verify_iarr, {16,  0xf6, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
			  0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x3f, 0xc7} },
	{ARRAY_SIZE(vl_iarr), 0x7f, 0x9, F_SA_HIGH | F_D_IN | FF_MEDIA_IO,
	    resp_read_dt0, vl_iarr,	/* VARIABLE LENGTH, READ(32) */
	    {32,  0xc7, 0, 0, 0, 0, 0x3f, 0x18, 0x0, 0x9, 0xfe, 0, 0xff, 0xff,
	     0xff, 0xff} },
	{ARRAY_SIZE(reserve_iarr), 0x56, 0, F_D_OUT,
	    NULL, reserve_iarr,	/* RESERVE(10) <no response function> */
	    {10,  0xff, 0xff, 0xff, 0, 0, 0, 0xff, 0xff, 0xc7, 0, 0, 0, 0, 0,
	     0} },
	{ARRAY_SIZE(release_iarr), 0x57, 0, F_D_OUT,
	    NULL, release_iarr, /* RELEASE(10) <no response function> */
	    {10,  0x13, 0xff, 0xff, 0, 0, 0, 0xff, 0xff, 0xc7, 0, 0, 0, 0, 0,
	     0} },
/* 20 */
	{0, 0x1e, 0, 0, NULL, NULL, /* ALLOW REMOVAL */
	    {6,  0, 0, 0, 0x3, 0xc7, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0} },
	{0, 0x1, 0, 0, resp_start_stop, NULL, /* REWIND ?? */
	    {6,  0x1, 0, 0, 0, 0xc7, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0} },
	{0, 0, 0, F_INV_OP | FF_RESPOND, NULL, NULL, /* ATA_PT */
	    {0,  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0} },
	{0, 0x1d, F_D_OUT, 0, NULL, NULL,	/* SEND DIAGNOSTIC */
	    {6,  0xf7, 0, 0xff, 0xff, 0xc7, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0} },
	{0, 0x42, 0, F_D_OUT | FF_MEDIA_IO, resp_unmap, NULL, /* UNMAP */
	    {10,  0x1, 0, 0, 0, 0, 0x3f, 0xff, 0xff, 0xc7, 0, 0, 0, 0, 0, 0} },
/* 25 */
	{0, 0x3b, 0, F_D_OUT_MAYBE, resp_write_buffer, NULL,
	    {10,  0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xc7, 0, 0,
	     0, 0, 0, 0} },			/* WRITE_BUFFER */
	{ARRAY_SIZE(write_same_iarr), 0x41, 0, F_D_OUT_MAYBE | FF_MEDIA_IO,
	    resp_write_same_10, write_same_iarr,	/* WRITE SAME(10) */
		{10,  0xff, 0xff, 0xff, 0xff, 0xff, 0x3f, 0xff, 0xff, 0xc7, 0,
		 0, 0, 0, 0, 0} },
	{ARRAY_SIZE(sync_cache_iarr), 0x35, 0, F_SYNC_DELAY | F_M_ACCESS,
	    resp_sync_cache, sync_cache_iarr,
	    {10,  0x7, 0xff, 0xff, 0xff, 0xff, 0x3f, 0xff, 0xff, 0xc7, 0, 0,
	     0, 0, 0, 0} },			/* SYNC_CACHE (10) */
	{0, 0x89, 0, F_D_OUT | FF_MEDIA_IO, resp_comp_write, NULL,
	    {16,  0xf8, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0, 0,
	     0, 0xff, 0x3f, 0xc7} },		/* COMPARE AND WRITE */
	{ARRAY_SIZE(pre_fetch_iarr), 0x34, 0, F_SYNC_DELAY | FF_MEDIA_IO,
	    resp_pre_fetch, pre_fetch_iarr,
	    {10,  0x2, 0xff, 0xff, 0xff, 0xff, 0x3f, 0xff, 0xff, 0xc7, 0, 0,
	     0, 0, 0, 0} },			/* PRE-FETCH (10) */

/* 30 */
	{ARRAY_SIZE(zone_out_iarr), 0x94, 0x3, F_SA_LOW | F_M_ACCESS,
	    resp_open_zone, zone_out_iarr, /* ZONE_OUT(16), OPEN ZONE) */
		{16,  0x3 /* SA */, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
		 0xff, 0xff, 0x0, 0x0, 0xff, 0xff, 0x1, 0xc7} },
	{ARRAY_SIZE(zone_in_iarr), 0x95, 0x0, F_SA_LOW | F_M_ACCESS,
	    resp_report_zones, zone_in_iarr, /* ZONE_IN(16), REPORT ZONES) */
		{16,  0x0 /* SA */, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
		 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xbf, 0xc7} },
/* 31 */
	{0, 0x0, 0x0, F_D_OUT | FF_MEDIA_IO,
	    resp_atomic_write, NULL, /* ATOMIC WRITE 16 */
		{16,  0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
		 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff} },
/* sentinel */
	{0xff, 0, 0, 0, NULL, NULL,		/* terminating element */
	    {0,  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0} },
};

static int sdebug_num_hosts;
static int sdebug_add_host = DEF_NUM_HOST;  /* in sysfs this is relative */
static int sdebug_ato = DEF_ATO;
static int sdebug_cdb_len = DEF_CDB_LEN;
static int sdebug_jdelay = DEF_JDELAY;	/* if > 0 then unit is jiffies */
static int sdebug_dev_size_mb = DEF_DEV_SIZE_PRE_INIT;
static int sdebug_dif = DEF_DIF;
static int sdebug_dix = DEF_DIX;
static int sdebug_dsense = DEF_D_SENSE;
static int sdebug_every_nth = DEF_EVERY_NTH;
static int sdebug_fake_rw = DEF_FAKE_RW;
static unsigned int sdebug_guard = DEF_GUARD;
static int sdebug_host_max_queue;	/* per host */
static int sdebug_lowest_aligned = DEF_LOWEST_ALIGNED;
static int sdebug_max_luns = DEF_MAX_LUNS;
static int sdebug_max_queue = SDEBUG_CANQUEUE;	/* per submit queue */
static unsigned int sdebug_medium_error_start = OPT_MEDIUM_ERR_ADDR;
static int sdebug_medium_error_count = OPT_MEDIUM_ERR_NUM;
static int sdebug_ndelay = DEF_NDELAY;	/* if > 0 then unit is nanoseconds */
static int sdebug_no_lun_0 = DEF_NO_LUN_0;
static int sdebug_no_uld;
static int sdebug_num_parts = DEF_NUM_PARTS;
static int sdebug_num_tgts = DEF_NUM_TGTS; /* targets per host */
static int sdebug_opt_blks = DEF_OPT_BLKS;
static int sdebug_opts = DEF_OPTS;
static int sdebug_physblk_exp = DEF_PHYSBLK_EXP;
static int sdebug_opt_xferlen_exp = DEF_OPT_XFERLEN_EXP;
static int sdebug_ptype = DEF_PTYPE; /* SCSI peripheral device type */
static int sdebug_scsi_level = DEF_SCSI_LEVEL;
static int sdebug_sector_size = DEF_SECTOR_SIZE;
static int sdeb_tur_ms_to_ready = DEF_TUR_MS_TO_READY;
static int sdebug_virtual_gb = DEF_VIRTUAL_GB;
static int sdebug_vpd_use_hostno = DEF_VPD_USE_HOSTNO;
static unsigned int sdebug_lbpu = DEF_LBPU;
static unsigned int sdebug_lbpws = DEF_LBPWS;
static unsigned int sdebug_lbpws10 = DEF_LBPWS10;
static unsigned int sdebug_lbprz = DEF_LBPRZ;
static unsigned int sdebug_unmap_alignment = DEF_UNMAP_ALIGNMENT;
static unsigned int sdebug_unmap_granularity = DEF_UNMAP_GRANULARITY;
static unsigned int sdebug_unmap_max_blocks = DEF_UNMAP_MAX_BLOCKS;
static unsigned int sdebug_unmap_max_desc = DEF_UNMAP_MAX_DESC;
static unsigned int sdebug_write_same_length = DEF_WRITESAME_LENGTH;
static unsigned int sdebug_atomic_wr = DEF_ATOMIC_WR;
static unsigned int sdebug_atomic_wr_max_length = DEF_ATOMIC_WR_MAX_LENGTH;
static unsigned int sdebug_atomic_wr_align = DEF_ATOMIC_WR_ALIGN;
static unsigned int sdebug_atomic_wr_gran = DEF_ATOMIC_WR_GRAN;
static unsigned int sdebug_atomic_wr_max_length_bndry =
			DEF_ATOMIC_WR_MAX_LENGTH_BNDRY;
static unsigned int sdebug_atomic_wr_max_bndry = DEF_ATOMIC_WR_MAX_BNDRY;
static int sdebug_uuid_ctl = DEF_UUID_CTL;
static bool sdebug_random = DEF_RANDOM;
static bool sdebug_per_host_store = DEF_PER_HOST_STORE;
static bool sdebug_removable = DEF_REMOVABLE;
static bool sdebug_clustering;
static bool sdebug_host_lock = DEF_HOST_LOCK;
static bool sdebug_strict = DEF_STRICT;
static bool sdebug_any_injecting_opt;
static bool sdebug_no_rwlock;
static bool sdebug_verbose;
static bool have_dif_prot;
static bool write_since_sync;
static bool sdebug_statistics = DEF_STATISTICS;
static bool sdebug_wp;
static bool sdebug_allow_restart;
static enum {
	BLK_ZONED_NONE	= 0,
	BLK_ZONED_HA	= 1,
	BLK_ZONED_HM	= 2,
} sdeb_zbc_model = BLK_ZONED_NONE;
static char *sdeb_zbc_model_s;

enum sam_lun_addr_method {SAM_LUN_AM_PERIPHERAL = 0x0,
			  SAM_LUN_AM_FLAT = 0x1,
			  SAM_LUN_AM_LOGICAL_UNIT = 0x2,
			  SAM_LUN_AM_EXTENDED = 0x3};
static enum sam_lun_addr_method sdebug_lun_am = SAM_LUN_AM_PERIPHERAL;
static int sdebug_lun_am_i = (int)SAM_LUN_AM_PERIPHERAL;

static unsigned int sdebug_store_sectors;
static sector_t sdebug_capacity;	/* in sectors */

/* old BIOS stuff, kernel may get rid of them but some mode sense pages
   may still need them */
static int sdebug_heads;		/* heads per disk */
static int sdebug_cylinders_per;	/* cylinders per surface */
static int sdebug_sectors_per;		/* sectors per cylinder */

static LIST_HEAD(sdebug_host_list);
static DEFINE_MUTEX(sdebug_host_list_mutex);

static struct xarray per_store_arr;
static struct xarray *per_store_ap = &per_store_arr;
static int sdeb_first_idx = -1;		/* invalid index ==> none created */
static int sdeb_most_recent_idx = -1;
static DEFINE_RWLOCK(sdeb_fake_rw_lck);	/* need a RW lock when fake_rw=1 */

static unsigned long map_size;
static int num_aborts;
static int num_dev_resets;
static int num_target_resets;
static int num_bus_resets;
static int num_host_resets;
static int dix_writes;
static int dix_reads;
static int dif_errors;

/* ZBC global data */
static bool sdeb_zbc_in_use;	/* true for host-aware and host-managed disks */
static int sdeb_zbc_zone_cap_mb;
static int sdeb_zbc_zone_size_mb;
static int sdeb_zbc_max_open = DEF_ZBC_MAX_OPEN_ZONES;
static int sdeb_zbc_nr_conv = DEF_ZBC_NR_CONV_ZONES;

static int submit_queues = DEF_SUBMIT_QUEUES;  /* > 1 for multi-queue (mq) */
static int poll_queues; /* iouring iopoll interface.*/

static atomic_long_t writes_by_group_number[64];

static char sdebug_proc_name[] = MY_NAME;
static const char *my_name = MY_NAME;

static const struct bus_type pseudo_lld_bus;

static struct device_driver sdebug_driverfs_driver = {
	.name 		= sdebug_proc_name,
	.bus		= &pseudo_lld_bus,
};

static const int check_condition_result =
	SAM_STAT_CHECK_CONDITION;

static const int illegal_condition_result =
	(DID_ABORT << 16) | SAM_STAT_CHECK_CONDITION;

static const int device_qfull_result =
	(DID_ABORT << 16) | SAM_STAT_TASK_SET_FULL;

static const int condition_met_result = SAM_STAT_CONDITION_MET;

static struct dentry *sdebug_debugfs_root;
static ASYNC_DOMAIN_EXCLUSIVE(sdebug_async_domain);

static void sdebug_err_free(struct rcu_head *head)
{
	struct sdebug_err_inject *inject =
		container_of(head, typeof(*inject), rcu);

	kfree(inject);
}

static void sdebug_err_add(struct scsi_device *sdev, struct sdebug_err_inject *new)
{
	struct sdebug_dev_info *devip = (struct sdebug_dev_info *)sdev->hostdata;
	struct sdebug_err_inject *err;

	spin_lock(&devip->list_lock);
	list_for_each_entry_rcu(err, &devip->inject_err_list, list) {
		if (err->type == new->type && err->cmd == new->cmd) {
			list_del_rcu(&err->list);
			call_rcu(&err->rcu, sdebug_err_free);
		}
	}

	list_add_tail_rcu(&new->list, &devip->inject_err_list);
	spin_unlock(&devip->list_lock);
}

static int sdebug_err_remove(struct scsi_device *sdev, const char *buf, size_t count)
{
	struct sdebug_dev_info *devip = (struct sdebug_dev_info *)sdev->hostdata;
	struct sdebug_err_inject *err;
	int type;
	unsigned char cmd;

	if (sscanf(buf, "- %d %hhx", &type, &cmd) != 2) {
		kfree(buf);
		return -EINVAL;
	}

	spin_lock(&devip->list_lock);
	list_for_each_entry_rcu(err, &devip->inject_err_list, list) {
		if (err->type == type && err->cmd == cmd) {
			list_del_rcu(&err->list);
			call_rcu(&err->rcu, sdebug_err_free);
			spin_unlock(&devip->list_lock);
			kfree(buf);
			return count;
		}
	}
	spin_unlock(&devip->list_lock);

	kfree(buf);
	return -EINVAL;
}

static int sdebug_error_show(struct seq_file *m, void *p)
{
	struct scsi_device *sdev = (struct scsi_device *)m->private;
	struct sdebug_dev_info *devip = (struct sdebug_dev_info *)sdev->hostdata;
	struct sdebug_err_inject *err;

	seq_puts(m, "Type\tCount\tCommand\n");

	rcu_read_lock();
	list_for_each_entry_rcu(err, &devip->inject_err_list, list) {
		switch (err->type) {
		case ERR_TMOUT_CMD:
		case ERR_ABORT_CMD_FAILED:
		case ERR_LUN_RESET_FAILED:
			seq_printf(m, "%d\t%d\t0x%x\n", err->type, err->cnt,
				err->cmd);
		break;

		case ERR_FAIL_QUEUE_CMD:
			seq_printf(m, "%d\t%d\t0x%x\t0x%x\n", err->type,
				err->cnt, err->cmd, err->queuecmd_ret);
		break;

		case ERR_FAIL_CMD:
			seq_printf(m, "%d\t%d\t0x%x\t0x%x 0x%x 0x%x 0x%x 0x%x 0x%x\n",
				err->type, err->cnt, err->cmd,
				err->host_byte, err->driver_byte,
				err->status_byte, err->sense_key,
				err->asc, err->asq);
		break;
		}
	}
	rcu_read_unlock();

	return 0;
}

static int sdebug_error_open(struct inode *inode, struct file *file)
{
	return single_open(file, sdebug_error_show, inode->i_private);
}

static ssize_t sdebug_error_write(struct file *file, const char __user *ubuf,
		size_t count, loff_t *ppos)
{
	char *buf;
	unsigned int inject_type;
	struct sdebug_err_inject *inject;
	struct scsi_device *sdev = (struct scsi_device *)file->f_inode->i_private;

	buf = kzalloc(count + 1, GFP_KERNEL);
	if (!buf)
		return -ENOMEM;

	if (copy_from_user(buf, ubuf, count)) {
		kfree(buf);
		return -EFAULT;
	}

	if (buf[0] == '-')
		return sdebug_err_remove(sdev, buf, count);

	if (sscanf(buf, "%d", &inject_type) != 1) {
		kfree(buf);
		return -EINVAL;
	}

	inject = kzalloc(sizeof(struct sdebug_err_inject), GFP_KERNEL);
	if (!inject) {
		kfree(buf);
		return -ENOMEM;
	}

	switch (inject_type) {
	case ERR_TMOUT_CMD:
	case ERR_ABORT_CMD_FAILED:
	case ERR_LUN_RESET_FAILED:
		if (sscanf(buf, "%d %d %hhx", &inject->type, &inject->cnt,
			   &inject->cmd) != 3)
			goto out_error;
	break;

	case ERR_FAIL_QUEUE_CMD:
		if (sscanf(buf, "%d %d %hhx %x", &inject->type, &inject->cnt,
			   &inject->cmd, &inject->queuecmd_ret) != 4)
			goto out_error;
	break;

	case ERR_FAIL_CMD:
		if (sscanf(buf, "%d %d %hhx %hhx %hhx %hhx %hhx %hhx %hhx",
			   &inject->type, &inject->cnt, &inject->cmd,
			   &inject->host_byte, &inject->driver_byte,
			   &inject->status_byte, &inject->sense_key,
			   &inject->asc, &inject->asq) != 9)
			goto out_error;
	break;

	default:
		goto out_error;
	break;
	}

	kfree(buf);
	sdebug_err_add(sdev, inject);

	return count;

out_error:
	kfree(buf);
	kfree(inject);
	return -EINVAL;
}

static const struct file_operations sdebug_error_fops = {
	.open	= sdebug_error_open,
	.read	= seq_read,
	.write	= sdebug_error_write,
	.release = single_release,
};

static int sdebug_target_reset_fail_show(struct seq_file *m, void *p)
{
	struct scsi_target *starget = (struct scsi_target *)m->private;
	struct sdebug_target_info *targetip =
		(struct sdebug_target_info *)starget->hostdata;

	if (targetip)
		seq_printf(m, "%c\n", targetip->reset_fail ? 'Y' : 'N');

	return 0;
}

static int sdebug_target_reset_fail_open(struct inode *inode, struct file *file)
{
	return single_open(file, sdebug_target_reset_fail_show, inode->i_private);
}

static ssize_t sdebug_target_reset_fail_write(struct file *file,
		const char __user *ubuf, size_t count, loff_t *ppos)
{
	int ret;
	struct scsi_target *starget =
		(struct scsi_target *)file->f_inode->i_private;
	struct sdebug_target_info *targetip =
		(struct sdebug_target_info *)starget->hostdata;

	if (targetip) {
		ret = kstrtobool_from_user(ubuf, count, &targetip->reset_fail);
		return ret < 0 ? ret : count;
	}
	return -ENODEV;
}

static const struct file_operations sdebug_target_reset_fail_fops = {
	.open	= sdebug_target_reset_fail_open,
	.read	= seq_read,
	.write	= sdebug_target_reset_fail_write,
	.release = single_release,
};

static int sdebug_target_alloc(struct scsi_target *starget)
{
	struct sdebug_target_info *targetip;

	targetip = kzalloc(sizeof(struct sdebug_target_info), GFP_KERNEL);
	if (!targetip)
		return -ENOMEM;

	async_synchronize_full_domain(&sdebug_async_domain);

	targetip->debugfs_entry = debugfs_create_dir(dev_name(&starget->dev),
				sdebug_debugfs_root);

	debugfs_create_file("fail_reset", 0600, targetip->debugfs_entry, starget,
				&sdebug_target_reset_fail_fops);

	starget->hostdata = targetip;

	return 0;
}

static void sdebug_tartget_cleanup_async(void *data, async_cookie_t cookie)
{
	struct sdebug_target_info *targetip = data;

	debugfs_remove(targetip->debugfs_entry);
	kfree(targetip);
}

static void sdebug_target_destroy(struct scsi_target *starget)
{
	struct sdebug_target_info *targetip;

	targetip = (struct sdebug_target_info *)starget->hostdata;
	if (targetip) {
		starget->hostdata = NULL;
		async_schedule_domain(sdebug_tartget_cleanup_async, targetip,
				&sdebug_async_domain);
	}
}

/* Only do the extra work involved in logical block provisioning if one or
 * more of the lbpu, lbpws or lbpws10 parameters are given and we are doing
 * real reads and writes (i.e. not skipping them for speed).
 */
static inline bool scsi_debug_lbp(void)
{
	return 0 == sdebug_fake_rw &&
		(sdebug_lbpu || sdebug_lbpws || sdebug_lbpws10);
}

static inline bool scsi_debug_atomic_write(void)
{
	return sdebug_fake_rw == 0 && sdebug_atomic_wr;
}

static void *lba2fake_store(struct sdeb_store_info *sip,
			    unsigned long long lba)
{
	struct sdeb_store_info *lsip = sip;

	lba = do_div(lba, sdebug_store_sectors);
	if (!sip || !sip->storep) {
		WARN_ON_ONCE(true);
		lsip = xa_load(per_store_ap, 0);  /* should never be NULL */
	}
	return lsip->storep + lba * sdebug_sector_size;
}

static struct t10_pi_tuple *dif_store(struct sdeb_store_info *sip,
				      sector_t sector)
{
	sector = sector_div(sector, sdebug_store_sectors);

	return sip->dif_storep + sector;
}

static void sdebug_max_tgts_luns(void)
{
	struct sdebug_host_info *sdbg_host;
	struct Scsi_Host *hpnt;

	mutex_lock(&sdebug_host_list_mutex);
	list_for_each_entry(sdbg_host, &sdebug_host_list, host_list) {
		hpnt = sdbg_host->shost;
		if ((hpnt->this_id >= 0) &&
		    (sdebug_num_tgts > hpnt->this_id))
			hpnt->max_id = sdebug_num_tgts + 1;
		else
			hpnt->max_id = sdebug_num_tgts;
		/* sdebug_max_luns; */
		hpnt->max_lun = SCSI_W_LUN_REPORT_LUNS + 1;
	}
	mutex_unlock(&sdebug_host_list_mutex);
}

enum sdeb_cmd_data {SDEB_IN_DATA = 0, SDEB_IN_CDB = 1};

/* Set in_bit to -1 to indicate no bit position of invalid field */
static void mk_sense_invalid_fld(struct scsi_cmnd *scp,
				 enum sdeb_cmd_data c_d,
				 int in_byte, int in_bit)
{
	unsigned char *sbuff;
	u8 sks[4];
	int sl, asc;

	sbuff = scp->sense_buffer;
	if (!sbuff) {
		sdev_printk(KERN_ERR, scp->device,
			    "%s: sense_buffer is NULL\n", __func__);
		return;
	}
	asc = c_d ? INVALID_FIELD_IN_CDB : INVALID_FIELD_IN_PARAM_LIST;
	memset(sbuff, 0, SCSI_SENSE_BUFFERSIZE);
	scsi_build_sense(scp, sdebug_dsense, ILLEGAL_REQUEST, asc, 0);
	memset(sks, 0, sizeof(sks));
	sks[0] = 0x80;
	if (c_d)
		sks[0] |= 0x40;
	if (in_bit >= 0) {
		sks[0] |= 0x8;
		sks[0] |= 0x7 & in_bit;
	}
	put_unaligned_be16(in_byte, sks + 1);
	if (sdebug_dsense) {
		sl = sbuff[7] + 8;
		sbuff[7] = sl;
		sbuff[sl] = 0x2;
		sbuff[sl + 1] = 0x6;
		memcpy(sbuff + sl + 4, sks, 3);
	} else
		memcpy(sbuff + 15, sks, 3);
	if (sdebug_verbose)
		sdev_printk(KERN_INFO, scp->device, "%s:  [sense_key,asc,ascq"
			    "]: [0x5,0x%x,0x0] %c byte=%d, bit=%d\n",
			    my_name, asc, c_d ? 'C' : 'D', in_byte, in_bit);
}

static void mk_sense_buffer(struct scsi_cmnd *scp, int key, int asc, int asq)
{
	if (!scp->sense_buffer) {
		sdev_printk(KERN_ERR, scp->device,
			    "%s: sense_buffer is NULL\n", __func__);
		return;
	}
	memset(scp->sense_buffer, 0, SCSI_SENSE_BUFFERSIZE);

	scsi_build_sense(scp, sdebug_dsense, key, asc, asq);

	if (sdebug_verbose)
		sdev_printk(KERN_INFO, scp->device,
			    "%s:  [sense_key,asc,ascq]: [0x%x,0x%x,0x%x]\n",
			    my_name, key, asc, asq);
}

static void mk_sense_invalid_opcode(struct scsi_cmnd *scp)
{
	mk_sense_buffer(scp, ILLEGAL_REQUEST, INVALID_OPCODE, 0);
}

static int scsi_debug_ioctl(struct scsi_device *dev, unsigned int cmd,
			    void __user *arg)
{
	if (sdebug_verbose) {
		if (0x1261 == cmd)
			sdev_printk(KERN_INFO, dev,
				    "%s: BLKFLSBUF [0x1261]\n", __func__);
		else if (0x5331 == cmd)
			sdev_printk(KERN_INFO, dev,
				    "%s: CDROM_GET_CAPABILITY [0x5331]\n",
				    __func__);
		else
			sdev_printk(KERN_INFO, dev, "%s: cmd=0x%x\n",
				    __func__, cmd);
	}
	return -EINVAL;
	/* return -ENOTTY; // correct return but upsets fdisk */
}

static void config_cdb_len(struct scsi_device *sdev)
{
	switch (sdebug_cdb_len) {
	case 6:	/* suggest 6 byte READ, WRITE and MODE SENSE/SELECT */
		sdev->use_10_for_rw = false;
		sdev->use_16_for_rw = false;
		sdev->use_10_for_ms = false;
		break;
	case 10: /* suggest 10 byte RWs and 6 byte MODE SENSE/SELECT */
		sdev->use_10_for_rw = true;
		sdev->use_16_for_rw = false;
		sdev->use_10_for_ms = false;
		break;
	case 12: /* suggest 10 byte RWs and 10 byte MODE SENSE/SELECT */
		sdev->use_10_for_rw = true;
		sdev->use_16_for_rw = false;
		sdev->use_10_for_ms = true;
		break;
	case 16:
		sdev->use_10_for_rw = false;
		sdev->use_16_for_rw = true;
		sdev->use_10_for_ms = true;
		break;
	case 32: /* No knobs to suggest this so same as 16 for now */
		sdev->use_10_for_rw = false;
		sdev->use_16_for_rw = true;
		sdev->use_10_for_ms = true;
		break;
	default:
		pr_warn("unexpected cdb_len=%d, force to 10\n",
			sdebug_cdb_len);
		sdev->use_10_for_rw = true;
		sdev->use_16_for_rw = false;
		sdev->use_10_for_ms = false;
		sdebug_cdb_len = 10;
		break;
	}
}

static void all_config_cdb_len(void)
{
	struct sdebug_host_info *sdbg_host;
	struct Scsi_Host *shost;
	struct scsi_device *sdev;

	mutex_lock(&sdebug_host_list_mutex);
	list_for_each_entry(sdbg_host, &sdebug_host_list, host_list) {
		shost = sdbg_host->shost;
		shost_for_each_device(sdev, shost) {
			config_cdb_len(sdev);
		}
	}
	mutex_unlock(&sdebug_host_list_mutex);
}

static void clear_luns_changed_on_target(struct sdebug_dev_info *devip)
{
	struct sdebug_host_info *sdhp = devip->sdbg_host;
	struct sdebug_dev_info *dp;

	list_for_each_entry(dp, &sdhp->dev_info_list, dev_list) {
		if ((devip->sdbg_host == dp->sdbg_host) &&
		    (devip->target == dp->target)) {
			clear_bit(SDEBUG_UA_LUNS_CHANGED, dp->uas_bm);
		}
	}
}

static int make_ua(struct scsi_cmnd *scp, struct sdebug_dev_info *devip)
{
	int k;

	k = find_first_bit(devip->uas_bm, SDEBUG_NUM_UAS);
	if (k != SDEBUG_NUM_UAS) {
		const char *cp = NULL;

		switch (k) {
		case SDEBUG_UA_POR:
			mk_sense_buffer(scp, UNIT_ATTENTION, UA_RESET_ASC,
					POWER_ON_RESET_ASCQ);
			if (sdebug_verbose)
				cp = "power on reset";
			break;
		case SDEBUG_UA_POOCCUR:
			mk_sense_buffer(scp, UNIT_ATTENTION, UA_RESET_ASC,
					POWER_ON_OCCURRED_ASCQ);
			if (sdebug_verbose)
				cp = "power on occurred";
			break;
		case SDEBUG_UA_BUS_RESET:
			mk_sense_buffer(scp, UNIT_ATTENTION, UA_RESET_ASC,
					BUS_RESET_ASCQ);
			if (sdebug_verbose)
				cp = "bus reset";
			break;
		case SDEBUG_UA_MODE_CHANGED:
			mk_sense_buffer(scp, UNIT_ATTENTION, UA_CHANGED_ASC,
					MODE_CHANGED_ASCQ);
			if (sdebug_verbose)
				cp = "mode parameters changed";
			break;
		case SDEBUG_UA_CAPACITY_CHANGED:
			mk_sense_buffer(scp, UNIT_ATTENTION, UA_CHANGED_ASC,
					CAPACITY_CHANGED_ASCQ);
			if (sdebug_verbose)
				cp = "capacity data changed";
			break;
		case SDEBUG_UA_MICROCODE_CHANGED:
			mk_sense_buffer(scp, UNIT_ATTENTION,
					TARGET_CHANGED_ASC,
					MICROCODE_CHANGED_ASCQ);
			if (sdebug_verbose)
				cp = "microcode has been changed";
			break;
		case SDEBUG_UA_MICROCODE_CHANGED_WO_RESET:
			mk_sense_buffer(scp, UNIT_ATTENTION,
					TARGET_CHANGED_ASC,
					MICROCODE_CHANGED_WO_RESET_ASCQ);
			if (sdebug_verbose)
				cp = "microcode has been changed without reset";
			break;
		case SDEBUG_UA_LUNS_CHANGED:
			/*
			 * SPC-3 behavior is to report a UNIT ATTENTION with
			 * ASC/ASCQ REPORTED LUNS DATA HAS CHANGED on every LUN
			 * on the target, until a REPORT LUNS command is
			 * received.  SPC-4 behavior is to report it only once.
			 * NOTE:  sdebug_scsi_level does not use the same
			 * values as struct scsi_device->scsi_level.
			 */
			if (sdebug_scsi_level >= 6)	/* SPC-4 and above */
				clear_luns_changed_on_target(devip);
			mk_sense_buffer(scp, UNIT_ATTENTION,
					TARGET_CHANGED_ASC,
					LUNS_CHANGED_ASCQ);
			if (sdebug_verbose)
				cp = "reported luns data has changed";
			break;
		default:
			pr_warn("unexpected unit attention code=%d\n", k);
			if (sdebug_verbose)
				cp = "unknown";
			break;
		}
		clear_bit(k, devip->uas_bm);
		if (sdebug_verbose)
			sdev_printk(KERN_INFO, scp->device,
				   "%s reports: Unit attention: %s\n",
				   my_name, cp);
		return check_condition_result;
	}
	return 0;
}

/* Build SCSI "data-in" buffer. Returns 0 if ok else (DID_ERROR << 16). */
static int fill_from_dev_buffer(struct scsi_cmnd *scp, unsigned char *arr,
				int arr_len)
{
	int act_len;
	struct scsi_data_buffer *sdb = &scp->sdb;

	if (!sdb->length)
		return 0;
	if (scp->sc_data_direction != DMA_FROM_DEVICE)
		return DID_ERROR << 16;

	act_len = sg_copy_from_buffer(sdb->table.sgl, sdb->table.nents,
				      arr, arr_len);
	scsi_set_resid(scp, scsi_bufflen(scp) - act_len);

	return 0;
}

/* Partial build of SCSI "data-in" buffer. Returns 0 if ok else
 * (DID_ERROR << 16). Can write to offset in data-in buffer. If multiple
 * calls, not required to write in ascending offset order. Assumes resid
 * set to scsi_bufflen() prior to any calls.
 */
static int p_fill_from_dev_buffer(struct scsi_cmnd *scp, const void *arr,
				  int arr_len, unsigned int off_dst)
{
	unsigned int act_len, n;
	struct scsi_data_buffer *sdb = &scp->sdb;
	off_t skip = off_dst;

	if (sdb->length <= off_dst)
		return 0;
	if (scp->sc_data_direction != DMA_FROM_DEVICE)
		return DID_ERROR << 16;

	act_len = sg_pcopy_from_buffer(sdb->table.sgl, sdb->table.nents,
				       arr, arr_len, skip);
	pr_debug("%s: off_dst=%u, scsi_bufflen=%u, act_len=%u, resid=%d\n",
		 __func__, off_dst, scsi_bufflen(scp), act_len,
		 scsi_get_resid(scp));
	n = scsi_bufflen(scp) - (off_dst + act_len);
	scsi_set_resid(scp, min_t(u32, scsi_get_resid(scp), n));
	return 0;
}

/* Fetches from SCSI "data-out" buffer. Returns number of bytes fetched into
 * 'arr' or -1 if error.
 */
static int fetch_to_dev_buffer(struct scsi_cmnd *scp, unsigned char *arr,
			       int arr_len)
{
	if (!scsi_bufflen(scp))
		return 0;
	if (scp->sc_data_direction != DMA_TO_DEVICE)
		return -1;

	return scsi_sg_copy_to_buffer(scp, arr, arr_len);
}


static char sdebug_inq_vendor_id[9] = "Linux   ";
static char sdebug_inq_product_id[17] = "scsi_debug      ";
static char sdebug_inq_product_rev[5] = SDEBUG_VERSION;
/* Use some locally assigned NAAs for SAS addresses. */
static const u64 naa3_comp_a = 0x3222222000000000ULL;
static const u64 naa3_comp_b = 0x3333333000000000ULL;
static const u64 naa3_comp_c = 0x3111111000000000ULL;

/* Device identification VPD page. Returns number of bytes placed in arr */
static int inquiry_vpd_83(unsigned char *arr, int port_group_id,
			  int target_dev_id, int dev_id_num,
			  const char *dev_id_str, int dev_id_str_len,
			  const uuid_t *lu_name)
{
	int num, port_a;
	char b[32];

	port_a = target_dev_id + 1;
	/* T10 vendor identifier field format (faked) */
	arr[0] = 0x2;	/* ASCII */
	arr[1] = 0x1;
	arr[2] = 0x0;
	memcpy(&arr[4], sdebug_inq_vendor_id, 8);
	memcpy(&arr[12], sdebug_inq_product_id, 16);
	memcpy(&arr[28], dev_id_str, dev_id_str_len);
	num = 8 + 16 + dev_id_str_len;
	arr[3] = num;
	num += 4;
	if (dev_id_num >= 0) {
		if (sdebug_uuid_ctl) {
			/* Locally assigned UUID */
			arr[num++] = 0x1;  /* binary (not necessarily sas) */
			arr[num++] = 0xa;  /* PIV=0, lu, naa */
			arr[num++] = 0x0;
			arr[num++] = 0x12;
			arr[num++] = 0x10; /* uuid type=1, locally assigned */
			arr[num++] = 0x0;
			memcpy(arr + num, lu_name, 16);
			num += 16;
		} else {
			/* NAA-3, Logical unit identifier (binary) */
			arr[num++] = 0x1;  /* binary (not necessarily sas) */
			arr[num++] = 0x3;  /* PIV=0, lu, naa */
			arr[num++] = 0x0;
			arr[num++] = 0x8;
			put_unaligned_be64(naa3_comp_b + dev_id_num, arr + num);
			num += 8;
		}
		/* Target relative port number */
		arr[num++] = 0x61;	/* proto=sas, binary */
		arr[num++] = 0x94;	/* PIV=1, target port, rel port */
		arr[num++] = 0x0;	/* reserved */
		arr[num++] = 0x4;	/* length */
		arr[num++] = 0x0;	/* reserved */
		arr[num++] = 0x0;	/* reserved */
		arr[num++] = 0x0;
		arr[num++] = 0x1;	/* relative port A */
	}
	/* NAA-3, Target port identifier */
	arr[num++] = 0x61;	/* proto=sas, binary */
	arr[num++] = 0x93;	/* piv=1, target port, naa */
	arr[num++] = 0x0;
	arr[num++] = 0x8;
	put_unaligned_be64(naa3_comp_a + port_a, arr + num);
	num += 8;
	/* NAA-3, Target port group identifier */
	arr[num++] = 0x61;	/* proto=sas, binary */
	arr[num++] = 0x95;	/* piv=1, target port group id */
	arr[num++] = 0x0;
	arr[num++] = 0x4;
	arr[num++] = 0;
	arr[num++] = 0;
	put_unaligned_be16(port_group_id, arr + num);
	num += 2;
	/* NAA-3, Target device identifier */
	arr[num++] = 0x61;	/* proto=sas, binary */
	arr[num++] = 0xa3;	/* piv=1, target device, naa */
	arr[num++] = 0x0;
	arr[num++] = 0x8;
	put_unaligned_be64(naa3_comp_a + target_dev_id, arr + num);
	num += 8;
	/* SCSI name string: Target device identifier */
	arr[num++] = 0x63;	/* proto=sas, UTF-8 */
	arr[num++] = 0xa8;	/* piv=1, target device, SCSI name string */
	arr[num++] = 0x0;
	arr[num++] = 24;
	memcpy(arr + num, "naa.32222220", 12);
	num += 12;
	snprintf(b, sizeof(b), "%08X", target_dev_id);
	memcpy(arr + num, b, 8);
	num += 8;
	memset(arr + num, 0, 4);
	num += 4;
	return num;
}

static unsigned char vpd84_data[] = {
/* from 4th byte */ 0x22,0x22,0x22,0x0,0xbb,0x0,
    0x22,0x22,0x22,0x0,0xbb,0x1,
    0x22,0x22,0x22,0x0,0xbb,0x2,
};

/*  Software interface identification VPD page */
static int inquiry_vpd_84(unsigned char *arr)
{
	memcpy(arr, vpd84_data, sizeof(vpd84_data));
	return sizeof(vpd84_data);
}

/* Management network addresses VPD page */
static int inquiry_vpd_85(unsigned char *arr)
{
	int num = 0;
	const char *na1 = "https://www.kernel.org/config";
	const char *na2 = "http://www.kernel.org/log";
	int plen, olen;

	arr[num++] = 0x1;	/* lu, storage config */
	arr[num++] = 0x0;	/* reserved */
	arr[num++] = 0x0;
	olen = strlen(na1);
	plen = olen + 1;
	if (plen % 4)
		plen = ((plen / 4) + 1) * 4;
	arr[num++] = plen;	/* length, null termianted, padded */
	memcpy(arr + num, na1, olen);
	memset(arr + num + olen, 0, plen - olen);
	num += plen;

	arr[num++] = 0x4;	/* lu, logging */
	arr[num++] = 0x0;	/* reserved */
	arr[num++] = 0x0;
	olen = strlen(na2);
	plen = olen + 1;
	if (plen % 4)
		plen = ((plen / 4) + 1) * 4;
	arr[num++] = plen;	/* length, null terminated, padded */
	memcpy(arr + num, na2, olen);
	memset(arr + num + olen, 0, plen - olen);
	num += plen;

	return num;
}

/* SCSI ports VPD page */
static int inquiry_vpd_88(unsigned char *arr, int target_dev_id)
{
	int num = 0;
	int port_a, port_b;

	port_a = target_dev_id + 1;
	port_b = port_a + 1;
	arr[num++] = 0x0;	/* reserved */
	arr[num++] = 0x0;	/* reserved */
	arr[num++] = 0x0;
	arr[num++] = 0x1;	/* relative port 1 (primary) */
	memset(arr + num, 0, 6);
	num += 6;
	arr[num++] = 0x0;
	arr[num++] = 12;	/* length tp descriptor */
	/* naa-5 target port identifier (A) */
	arr[num++] = 0x61;	/* proto=sas, binary */
	arr[num++] = 0x93;	/* PIV=1, target port, NAA */
	arr[num++] = 0x0;	/* reserved */
	arr[num++] = 0x8;	/* length */
	put_unaligned_be64(naa3_comp_a + port_a, arr + num);
	num += 8;
	arr[num++] = 0x0;	/* reserved */
	arr[num++] = 0x0;	/* reserved */
	arr[num++] = 0x0;
	arr[num++] = 0x2;	/* relative port 2 (secondary) */
	memset(arr + num, 0, 6);
	num += 6;
	arr[num++] = 0x0;
	arr[num++] = 12;	/* length tp descriptor */
	/* naa-5 target port identifier (B) */
	arr[num++] = 0x61;	/* proto=sas, binary */
	arr[num++] = 0x93;	/* PIV=1, target port, NAA */
	arr[num++] = 0x0;	/* reserved */
	arr[num++] = 0x8;	/* length */
	put_unaligned_be64(naa3_comp_a + port_b, arr + num);
	num += 8;

	return num;
}


static unsigned char vpd89_data[] = {
/* from 4th byte */ 0,0,0,0,
'l','i','n','u','x',' ',' ',' ',
'S','A','T',' ','s','c','s','i','_','d','e','b','u','g',' ',' ',
'1','2','3','4',
0x34,0,0,0,1,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,
0xec,0,0,0,
0x5a,0xc,0xff,0x3f,0x37,0xc8,0x10,0,0,0,0,0,0x3f,0,0,0,
0,0,0,0,0x58,0x58,0x58,0x58,0x58,0x58,0x58,0x58,0x20,0x20,0x20,0x20,
0x20,0x20,0x20,0x20,0x20,0x20,0x20,0x20,0,0,0,0x40,0x4,0,0x2e,0x33,
0x38,0x31,0x20,0x20,0x20,0x20,0x54,0x53,0x38,0x33,0x30,0x30,0x33,0x31,
0x53,0x41,
0x20,0x20,0x20,0x20,0x20,0x20,0x20,0x20,0x20,0x20,0x20,0x20,0x20,0x20,
0x20,0x20,
0x20,0x20,0x20,0x20,0x20,0x20,0x20,0x20,0x20,0x20,0x20,0x20,0x20,0x20,
0x10,0x80,
0,0,0,0x2f,0,0,0,0x2,0,0x2,0x7,0,0xff,0xff,0x1,0,
0x3f,0,0xc1,0xff,0x3e,0,0x10,0x1,0xb0,0xf8,0x50,0x9,0,0,0x7,0,
0x3,0,0x78,0,0x78,0,0xf0,0,0x78,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0x2,0,0,0,0,0,0,0,
0x7e,0,0x1b,0,0x6b,0x34,0x1,0x7d,0x3,0x40,0x69,0x34,0x1,0x3c,0x3,0x40,
0x7f,0x40,0,0,0,0,0xfe,0xfe,0,0,0,0,0,0xfe,0,0,
0,0,0,0,0,0,0,0,0xb0,0xf8,0x50,0x9,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0x1,0,0xb0,0xf8,0x50,0x9,0xb0,0xf8,0x50,0x9,0x20,0x20,0x2,0,0xb6,0x42,
0,0x80,0x8a,0,0x6,0x3c,0xa,0x3c,0xff,0xff,0xc6,0x7,0,0x1,0,0x8,
0xf0,0xf,0,0x10,0x2,0,0x30,0,0,0,0,0,0,0,0x6,0xfe,
0,0,0x2,0,0x50,0,0x8a,0,0x4f,0x95,0,0,0x21,0,0xb,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0xa5,0x51,
};

/* ATA Information VPD page */
static int inquiry_vpd_89(unsigned char *arr)
{
	memcpy(arr, vpd89_data, sizeof(vpd89_data));
	return sizeof(vpd89_data);
}


static unsigned char vpdb0_data[] = {
	/* from 4th byte */ 0,0,0,4, 0,0,0x4,0, 0,0,0,64,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
};

/* Block limits VPD page (SBC-3) */
static int inquiry_vpd_b0(unsigned char *arr)
{
	unsigned int gran;

	memcpy(arr, vpdb0_data, sizeof(vpdb0_data));

	/* Optimal transfer length granularity */
	if (sdebug_opt_xferlen_exp != 0 &&
	    sdebug_physblk_exp < sdebug_opt_xferlen_exp)
		gran = 1 << sdebug_opt_xferlen_exp;
	else
		gran = 1 << sdebug_physblk_exp;
	put_unaligned_be16(gran, arr + 2);

	/* Maximum Transfer Length */
	if (sdebug_store_sectors > 0x400)
		put_unaligned_be32(sdebug_store_sectors, arr + 4);

	/* Optimal Transfer Length */
	put_unaligned_be32(sdebug_opt_blks, &arr[8]);

	if (sdebug_lbpu) {
		/* Maximum Unmap LBA Count */
		put_unaligned_be32(sdebug_unmap_max_blocks, &arr[16]);

		/* Maximum Unmap Block Descriptor Count */
		put_unaligned_be32(sdebug_unmap_max_desc, &arr[20]);
	}

	/* Unmap Granularity Alignment */
	if (sdebug_unmap_alignment) {
		put_unaligned_be32(sdebug_unmap_alignment, &arr[28]);
		arr[28] |= 0x80; /* UGAVALID */
	}

	/* Optimal Unmap Granularity */
	put_unaligned_be32(sdebug_unmap_granularity, &arr[24]);

	/* Maximum WRITE SAME Length */
	put_unaligned_be64(sdebug_write_same_length, &arr[32]);

	if (sdebug_atomic_wr) {
		put_unaligned_be32(sdebug_atomic_wr_max_length, &arr[40]);
		put_unaligned_be32(sdebug_atomic_wr_align, &arr[44]);
		put_unaligned_be32(sdebug_atomic_wr_gran, &arr[48]);
		put_unaligned_be32(sdebug_atomic_wr_max_length_bndry, &arr[52]);
		put_unaligned_be32(sdebug_atomic_wr_max_bndry, &arr[56]);
	}

	return 0x3c; /* Mandatory page length for Logical Block Provisioning */
}

/* Block device characteristics VPD page (SBC-3) */
static int inquiry_vpd_b1(struct sdebug_dev_info *devip, unsigned char *arr)
{
	memset(arr, 0, 0x3c);
	arr[0] = 0;
	arr[1] = 1;	/* non rotating medium (e.g. solid state) */
	arr[2] = 0;
	arr[3] = 5;	/* less than 1.8" */

	return 0x3c;
}

/* Logical block provisioning VPD page (SBC-4) */
static int inquiry_vpd_b2(unsigned char *arr)
{
	memset(arr, 0, 0x4);
	arr[0] = 0;			/* threshold exponent */
	if (sdebug_lbpu)
		arr[1] = 1 << 7;
	if (sdebug_lbpws)
		arr[1] |= 1 << 6;
	if (sdebug_lbpws10)
		arr[1] |= 1 << 5;
	if (sdebug_lbprz && scsi_debug_lbp())
		arr[1] |= (sdebug_lbprz & 0x7) << 2;  /* sbc4r07 and later */
	/* anc_sup=0; dp=0 (no provisioning group descriptor) */
	/* minimum_percentage=0; provisioning_type=0 (unknown) */
	/* threshold_percentage=0 */
	return 0x4;
}

/* Zoned block device characteristics VPD page (ZBC mandatory) */
static int inquiry_vpd_b6(struct sdebug_dev_info *devip, unsigned char *arr)
{
	memset(arr, 0, 0x3c);
	arr[0] = 0x1; /* set URSWRZ (unrestricted read in seq. wr req zone) */
	/*
	 * Set Optimal number of open sequential write preferred zones and
	 * Optimal number of non-sequentially written sequential write
	 * preferred zones fields to 'not reported' (0xffffffff). Leave other
	 * fields set to zero, apart from Max. number of open swrz_s field.
	 */
	put_unaligned_be32(0xffffffff, &arr[4]);
	put_unaligned_be32(0xffffffff, &arr[8]);
	if (sdeb_zbc_model == BLK_ZONED_HM && devip->max_open)
		put_unaligned_be32(devip->max_open, &arr[12]);
	else
		put_unaligned_be32(0xffffffff, &arr[12]);
	if (devip->zcap < devip->zsize) {
		arr[19] = ZBC_CONSTANT_ZONE_START_OFFSET;
		put_unaligned_be64(devip->zsize, &arr[20]);
	} else {
		arr[19] = 0;
	}
	return 0x3c;
}

#define SDEBUG_BLE_LEN_AFTER_B4 28	/* thus vpage 32 bytes long */

enum { MAXIMUM_NUMBER_OF_STREAMS = 6, PERMANENT_STREAM_COUNT = 5 };

/* Block limits extension VPD page (SBC-4) */
static int inquiry_vpd_b7(unsigned char *arrb4)
{
	memset(arrb4, 0, SDEBUG_BLE_LEN_AFTER_B4);
	arrb4[1] = 1; /* Reduced stream control support (RSCS) */
	put_unaligned_be16(MAXIMUM_NUMBER_OF_STREAMS, &arrb4[2]);
	return SDEBUG_BLE_LEN_AFTER_B4;
}

#define SDEBUG_LONG_INQ_SZ 96
#define SDEBUG_MAX_INQ_ARR_SZ 584

static int resp_inquiry(struct scsi_cmnd *scp, struct sdebug_dev_info *devip)
{
	unsigned char pq_pdt;
	unsigned char *arr;
	unsigned char *cmd = scp->cmnd;
	u32 alloc_len, n;
	int ret;
	bool have_wlun, is_disk, is_zbc, is_disk_zbc;

	alloc_len = get_unaligned_be16(cmd + 3);
	arr = kzalloc(SDEBUG_MAX_INQ_ARR_SZ, GFP_ATOMIC);
	if (! arr)
		return DID_REQUEUE << 16;
	is_disk = (sdebug_ptype == TYPE_DISK);
	is_zbc = devip->zoned;
	is_disk_zbc = (is_disk || is_zbc);
	have_wlun = scsi_is_wlun(scp->device->lun);
	if (have_wlun)
		pq_pdt = TYPE_WLUN;	/* present, wlun */
	else if (sdebug_no_lun_0 && (devip->lun == SDEBUG_LUN_0_VAL))
		pq_pdt = 0x7f;	/* not present, PQ=3, PDT=0x1f */
	else
		pq_pdt = (sdebug_ptype & 0x1f);
	arr[0] = pq_pdt;
	if (0x2 & cmd[1]) {  /* CMDDT bit set */
		mk_sense_invalid_fld(scp, SDEB_IN_CDB, 1, 1);
		kfree(arr);
		return check_condition_result;
	} else if (0x1 & cmd[1]) {  /* EVPD bit set */
		int lu_id_num, port_group_id, target_dev_id;
		u32 len;
		char lu_id_str[6];
		int host_no = devip->sdbg_host->shost->host_no;

		arr[1] = cmd[2];
		port_group_id = (((host_no + 1) & 0x7f) << 8) +
		    (devip->channel & 0x7f);
		if (sdebug_vpd_use_hostno == 0)
			host_no = 0;
		lu_id_num = have_wlun ? -1 : (((host_no + 1) * 2000) +
			    (devip->target * 1000) + devip->lun);
		target_dev_id = ((host_no + 1) * 2000) +
				 (devip->target * 1000) - 3;
		len = scnprintf(lu_id_str, 6, "%d", lu_id_num);
		if (0 == cmd[2]) { /* supported vital product data pages */
			n = 4;
			arr[n++] = 0x0;   /* this page */
			arr[n++] = 0x80;  /* unit serial number */
			arr[n++] = 0x83;  /* device identification */
			arr[n++] = 0x84;  /* software interface ident. */
			arr[n++] = 0x85;  /* management network addresses */
			arr[n++] = 0x86;  /* extended inquiry */
			arr[n++] = 0x87;  /* mode page policy */
			arr[n++] = 0x88;  /* SCSI ports */
			if (is_disk_zbc) {	  /* SBC or ZBC */
				arr[n++] = 0x89;  /* ATA information */
				arr[n++] = 0xb0;  /* Block limits */
				arr[n++] = 0xb1;  /* Block characteristics */
				if (is_disk)
					arr[n++] = 0xb2;  /* LB Provisioning */
				if (is_zbc)
					arr[n++] = 0xb6;  /* ZB dev. char. */
				arr[n++] = 0xb7;  /* Block limits extension */
			}
			arr[3] = n - 4;	  /* number of supported VPD pages */
		} else if (0x80 == cmd[2]) { /* unit serial number */
			arr[3] = len;
			memcpy(&arr[4], lu_id_str, len);
		} else if (0x83 == cmd[2]) { /* device identification */
			arr[3] = inquiry_vpd_83(&arr[4], port_group_id,
						target_dev_id, lu_id_num,
						lu_id_str, len,
						&devip->lu_name);
		} else if (0x84 == cmd[2]) { /* Software interface ident. */
			arr[3] = inquiry_vpd_84(&arr[4]);
		} else if (0x85 == cmd[2]) { /* Management network addresses */
			arr[3] = inquiry_vpd_85(&arr[4]);
		} else if (0x86 == cmd[2]) { /* extended inquiry */
			arr[3] = 0x3c;	/* number of following entries */
			if (sdebug_dif == T10_PI_TYPE3_PROTECTION)
				arr[4] = 0x4;	/* SPT: GRD_CHK:1 */
			else if (have_dif_prot)
				arr[4] = 0x5;   /* SPT: GRD_CHK:1, REF_CHK:1 */
			else
				arr[4] = 0x0;   /* no protection stuff */
			/*
			 * GROUP_SUP=1; HEADSUP=1 (HEAD OF QUEUE); ORDSUP=1
			 * (ORDERED queuing); SIMPSUP=1 (SIMPLE queuing).
			 */
			arr[5] = 0x17;
		} else if (0x87 == cmd[2]) { /* mode page policy */
			arr[3] = 0x8;	/* number of following entries */
			arr[4] = 0x2;	/* disconnect-reconnect mp */
			arr[6] = 0x80;	/* mlus, shared */
			arr[8] = 0x18;	 /* protocol specific lu */
			arr[10] = 0x82;	 /* mlus, per initiator port */
		} else if (0x88 == cmd[2]) { /* SCSI Ports */
			arr[3] = inquiry_vpd_88(&arr[4], target_dev_id);
		} else if (is_disk_zbc && 0x89 == cmd[2]) { /* ATA info */
			n = inquiry_vpd_89(&arr[4]);
			put_unaligned_be16(n, arr + 2);
		} else if (is_disk_zbc && 0xb0 == cmd[2]) { /* Block limits */
			arr[3] = inquiry_vpd_b0(&arr[4]);
		} else if (is_disk_zbc && 0xb1 == cmd[2]) { /* Block char. */
			arr[3] = inquiry_vpd_b1(devip, &arr[4]);
		} else if (is_disk && 0xb2 == cmd[2]) { /* LB Prov. */
			arr[3] = inquiry_vpd_b2(&arr[4]);
		} else if (is_zbc && cmd[2] == 0xb6) { /* ZB dev. charact. */
			arr[3] = inquiry_vpd_b6(devip, &arr[4]);
		} else if (cmd[2] == 0xb7) { /* block limits extension page */
			arr[3] = inquiry_vpd_b7(&arr[4]);
		} else {
			mk_sense_invalid_fld(scp, SDEB_IN_CDB, 2, -1);
			kfree(arr);
			return check_condition_result;
		}
		len = min_t(u32, get_unaligned_be16(arr + 2) + 4, alloc_len);
		ret = fill_from_dev_buffer(scp, arr,
			    min_t(u32, len, SDEBUG_MAX_INQ_ARR_SZ));
		kfree(arr);
		return ret;
	}
	/* drops through here for a standard inquiry */
	arr[1] = sdebug_removable ? 0x80 : 0;	/* Removable disk */
	arr[2] = sdebug_scsi_level;
	arr[3] = 2;    /* response_data_format==2 */
	arr[4] = SDEBUG_LONG_INQ_SZ - 5;
	arr[5] = (int)have_dif_prot;	/* PROTECT bit */
	if (sdebug_vpd_use_hostno == 0)
		arr[5] |= 0x10; /* claim: implicit TPGS */
	arr[6] = 0x10; /* claim: MultiP */
	/* arr[6] |= 0x40; ... claim: EncServ (enclosure services) */
	arr[7] = 0xa; /* claim: LINKED + CMDQUE */
	memcpy(&arr[8], sdebug_inq_vendor_id, 8);
	memcpy(&arr[16], sdebug_inq_product_id, 16);
	memcpy(&arr[32], sdebug_inq_product_rev, 4);
	/* Use Vendor Specific area to place driver date in ASCII hex */
	memcpy(&arr[36], sdebug_version_date, 8);
	/* version descriptors (2 bytes each) follow */
	put_unaligned_be16(0xc0, arr + 58);   /* SAM-6 no version claimed */
	put_unaligned_be16(0x5c0, arr + 60);  /* SPC-5 no version claimed */
	n = 62;
	if (is_disk) {		/* SBC-4 no version claimed */
		put_unaligned_be16(0x600, arr + n);
		n += 2;
	} else if (sdebug_ptype == TYPE_TAPE) {	/* SSC-4 rev 3 */
		put_unaligned_be16(0x525, arr + n);
		n += 2;
	} else if (is_zbc) {	/* ZBC BSR INCITS 536 revision 05 */
		put_unaligned_be16(0x624, arr + n);
		n += 2;
	}
	put_unaligned_be16(0x2100, arr + n);	/* SPL-4 no version claimed */
	ret = fill_from_dev_buffer(scp, arr,
			    min_t(u32, alloc_len, SDEBUG_LONG_INQ_SZ));
	kfree(arr);
	return ret;
}

/* See resp_iec_m_pg() for how this data is manipulated */
static unsigned char iec_m_pg[] = {0x1c, 0xa, 0x08, 0, 0, 0, 0, 0,
				   0, 0, 0x0, 0x0};

static int resp_requests(struct scsi_cmnd *scp,
			 struct sdebug_dev_info *devip)
{
	unsigned char *cmd = scp->cmnd;
	unsigned char arr[SCSI_SENSE_BUFFERSIZE];	/* assume >= 18 bytes */
	bool dsense = !!(cmd[1] & 1);
	u32 alloc_len = cmd[4];
	u32 len = 18;
	int stopped_state = atomic_read(&devip->stopped);

	memset(arr, 0, sizeof(arr));
	if (stopped_state > 0) {	/* some "pollable" data [spc6r02: 5.12.2] */
		if (dsense) {
			arr[0] = 0x72;
			arr[1] = NOT_READY;
			arr[2] = LOGICAL_UNIT_NOT_READY;
			arr[3] = (stopped_state == 2) ? 0x1 : 0x2;
			len = 8;
		} else {
			arr[0] = 0x70;
			arr[2] = NOT_READY;		/* NO_SENSE in sense_key */
			arr[7] = 0xa;			/* 18 byte sense buffer */
			arr[12] = LOGICAL_UNIT_NOT_READY;
			arr[13] = (stopped_state == 2) ? 0x1 : 0x2;
		}
	} else if ((iec_m_pg[2] & 0x4) && (6 == (iec_m_pg[3] & 0xf))) {
		/* Information exceptions control mode page: TEST=1, MRIE=6 */
		if (dsense) {
			arr[0] = 0x72;
			arr[1] = 0x0;		/* NO_SENSE in sense_key */
			arr[2] = THRESHOLD_EXCEEDED;
			arr[3] = 0xff;		/* Failure prediction(false) */
			len = 8;
		} else {
			arr[0] = 0x70;
			arr[2] = 0x0;		/* NO_SENSE in sense_key */
			arr[7] = 0xa;   	/* 18 byte sense buffer */
			arr[12] = THRESHOLD_EXCEEDED;
			arr[13] = 0xff;		/* Failure prediction(false) */
		}
	} else {	/* nothing to report */
		if (dsense) {
			len = 8;
			memset(arr, 0, len);
			arr[0] = 0x72;
		} else {
			memset(arr, 0, len);
			arr[0] = 0x70;
			arr[7] = 0xa;
		}
	}
	return fill_from_dev_buffer(scp, arr, min_t(u32, len, alloc_len));
}

static int resp_start_stop(struct scsi_cmnd *scp, struct sdebug_dev_info *devip)
{
	unsigned char *cmd = scp->cmnd;
	int power_cond, want_stop, stopped_state;
	bool changing;

	power_cond = (cmd[4] & 0xf0) >> 4;
	if (power_cond) {
		mk_sense_invalid_fld(scp, SDEB_IN_CDB, 4, 7);
		return check_condition_result;
	}
	want_stop = !(cmd[4] & 1);
	stopped_state = atomic_read(&devip->stopped);
	if (stopped_state == 2) {
		ktime_t now_ts = ktime_get_boottime();

		if (ktime_to_ns(now_ts) > ktime_to_ns(devip->create_ts)) {
			u64 diff_ns = ktime_to_ns(ktime_sub(now_ts, devip->create_ts));

			if (diff_ns >= ((u64)sdeb_tur_ms_to_ready * 1000000)) {
				/* tur_ms_to_ready timer extinguished */
				atomic_set(&devip->stopped, 0);
				stopped_state = 0;
			}
		}
		if (stopped_state == 2) {
			if (want_stop) {
				stopped_state = 1;	/* dummy up success */
			} else {	/* Disallow tur_ms_to_ready delay to be overridden */
				mk_sense_invalid_fld(scp, SDEB_IN_CDB, 4, 0 /* START bit */);
				return check_condition_result;
			}
		}
	}
	changing = (stopped_state != want_stop);
	if (changing)
		atomic_xchg(&devip->stopped, want_stop);
	if (!changing || (cmd[1] & 0x1))  /* state unchanged or IMMED bit set in cdb */
		return SDEG_RES_IMMED_MASK;
	else
		return 0;
}

static sector_t get_sdebug_capacity(void)
{
	static const unsigned int gibibyte = 1073741824;

	if (sdebug_virtual_gb > 0)
		return (sector_t)sdebug_virtual_gb *
			(gibibyte / sdebug_sector_size);
	else
		return sdebug_store_sectors;
}

#define SDEBUG_READCAP_ARR_SZ 8
static int resp_readcap(struct scsi_cmnd *scp,
			struct sdebug_dev_info *devip)
{
	unsigned char arr[SDEBUG_READCAP_ARR_SZ];
	unsigned int capac;

	/* following just in case virtual_gb changed */
	sdebug_capacity = get_sdebug_capacity();
	memset(arr, 0, SDEBUG_READCAP_ARR_SZ);
	if (sdebug_capacity < 0xffffffff) {
		capac = (unsigned int)sdebug_capacity - 1;
		put_unaligned_be32(capac, arr + 0);
	} else
		put_unaligned_be32(0xffffffff, arr + 0);
	put_unaligned_be16(sdebug_sector_size, arr + 6);
	return fill_from_dev_buffer(scp, arr, SDEBUG_READCAP_ARR_SZ);
}

#define SDEBUG_READCAP16_ARR_SZ 32
static int resp_readcap16(struct scsi_cmnd *scp,
			  struct sdebug_dev_info *devip)
{
	unsigned char *cmd = scp->cmnd;
	unsigned char arr[SDEBUG_READCAP16_ARR_SZ];
	u32 alloc_len;

	alloc_len = get_unaligned_be32(cmd + 10);
	/* following just in case virtual_gb changed */
	sdebug_capacity = get_sdebug_capacity();
	memset(arr, 0, SDEBUG_READCAP16_ARR_SZ);
	put_unaligned_be64((u64)(sdebug_capacity - 1), arr + 0);
	put_unaligned_be32(sdebug_sector_size, arr + 8);
	arr[13] = sdebug_physblk_exp & 0xf;
	arr[14] = (sdebug_lowest_aligned >> 8) & 0x3f;

	if (scsi_debug_lbp()) {
		arr[14] |= 0x80; /* LBPME */
		/* from sbc4r07, this LBPRZ field is 1 bit, but the LBPRZ in
		 * the LB Provisioning VPD page is 3 bits. Note that lbprz=2
		 * in the wider field maps to 0 in this field.
		 */
		if (sdebug_lbprz & 1)	/* precisely what the draft requires */
			arr[14] |= 0x40;
	}

	/*
	 * Since the scsi_debug READ CAPACITY implementation always reports the
	 * total disk capacity, set RC BASIS = 1 for host-managed ZBC devices.
	 */
	if (devip->zoned)
		arr[12] |= 1 << 4;

	arr[15] = sdebug_lowest_aligned & 0xff;

	if (have_dif_prot) {
		arr[12] = (sdebug_dif - 1) << 1; /* P_TYPE */
		arr[12] |= 1; /* PROT_EN */
	}

	return fill_from_dev_buffer(scp, arr,
			    min_t(u32, alloc_len, SDEBUG_READCAP16_ARR_SZ));
}

#define SDEBUG_MAX_TGTPGS_ARR_SZ 1412

static int resp_report_tgtpgs(struct scsi_cmnd *scp,
			      struct sdebug_dev_info *devip)
{
	unsigned char *cmd = scp->cmnd;
	unsigned char *arr;
	int host_no = devip->sdbg_host->shost->host_no;
	int port_group_a, port_group_b, port_a, port_b;
	u32 alen, n, rlen;
	int ret;

	alen = get_unaligned_be32(cmd + 6);
	arr = kzalloc(SDEBUG_MAX_TGTPGS_ARR_SZ, GFP_ATOMIC);
	if (! arr)
		return DID_REQUEUE << 16;
	/*
	 * EVPD page 0x88 states we have two ports, one
	 * real and a fake port with no device connected.
	 * So we create two port groups with one port each
	 * and set the group with port B to unavailable.
	 */
	port_a = 0x1; /* relative port A */
	port_b = 0x2; /* relative port B */
	port_group_a = (((host_no + 1) & 0x7f) << 8) +
			(devip->channel & 0x7f);
	port_group_b = (((host_no + 1) & 0x7f) << 8) +
			(devip->channel & 0x7f) + 0x80;

	/*
	 * The asymmetric access state is cycled according to the host_id.
	 */
	n = 4;
	if (sdebug_vpd_use_hostno == 0) {
		arr[n++] = host_no % 3; /* Asymm access state */
		arr[n++] = 0x0F; /* claim: all states are supported */
	} else {
		arr[n++] = 0x0; /* Active/Optimized path */
		arr[n++] = 0x01; /* only support active/optimized paths */
	}
	put_unaligned_be16(port_group_a, arr + n);
	n += 2;
	arr[n++] = 0;    /* Reserved */
	arr[n++] = 0;    /* Status code */
	arr[n++] = 0;    /* Vendor unique */
	arr[n++] = 0x1;  /* One port per group */
	arr[n++] = 0;    /* Reserved */
	arr[n++] = 0;    /* Reserved */
	put_unaligned_be16(port_a, arr + n);
	n += 2;
	arr[n++] = 3;    /* Port unavailable */
	arr[n++] = 0x08; /* claim: only unavailalbe paths are supported */
	put_unaligned_be16(port_group_b, arr + n);
	n += 2;
	arr[n++] = 0;    /* Reserved */
	arr[n++] = 0;    /* Status code */
	arr[n++] = 0;    /* Vendor unique */
	arr[n++] = 0x1;  /* One port per group */
	arr[n++] = 0;    /* Reserved */
	arr[n++] = 0;    /* Reserved */
	put_unaligned_be16(port_b, arr + n);
	n += 2;

	rlen = n - 4;
	put_unaligned_be32(rlen, arr + 0);

	/*
	 * Return the smallest value of either
	 * - The allocated length
	 * - The constructed command length
	 * - The maximum array size
	 */
	rlen = min(alen, n);
	ret = fill_from_dev_buffer(scp, arr,
			   min_t(u32, rlen, SDEBUG_MAX_TGTPGS_ARR_SZ));
	kfree(arr);
	return ret;
}

static int resp_rsup_opcodes(struct scsi_cmnd *scp,
			     struct sdebug_dev_info *devip)
{
	bool rctd;
	u8 reporting_opts, req_opcode, sdeb_i, supp;
	u16 req_sa, u;
	u32 alloc_len, a_len;
	int k, offset, len, errsts, count, bump, na;
	const struct opcode_info_t *oip;
	const struct opcode_info_t *r_oip;
	u8 *arr;
	u8 *cmd = scp->cmnd;

	rctd = !!(cmd[2] & 0x80);
	reporting_opts = cmd[2] & 0x7;
	req_opcode = cmd[3];
	req_sa = get_unaligned_be16(cmd + 4);
	alloc_len = get_unaligned_be32(cmd + 6);
	if (alloc_len < 4 || alloc_len > 0xffff) {
		mk_sense_invalid_fld(scp, SDEB_IN_CDB, 6, -1);
		return check_condition_result;
	}
	if (alloc_len > 8192)
		a_len = 8192;
	else
		a_len = alloc_len;
	arr = kzalloc((a_len < 256) ? 320 : a_len + 64, GFP_ATOMIC);
	if (NULL == arr) {
		mk_sense_buffer(scp, ILLEGAL_REQUEST, INSUFF_RES_ASC,
				INSUFF_RES_ASCQ);
		return check_condition_result;
	}
	switch (reporting_opts) {
	case 0:	/* all commands */
		/* count number of commands */
		for (count = 0, oip = opcode_info_arr;
		     oip->num_attached != 0xff; ++oip) {
			if (F_INV_OP & oip->flags)
				continue;
			count += (oip->num_attached + 1);
		}
		bump = rctd ? 20 : 8;
		put_unaligned_be32(count * bump, arr);
		for (offset = 4, oip = opcode_info_arr;
		     oip->num_attached != 0xff && offset < a_len; ++oip) {
			if (F_INV_OP & oip->flags)
				continue;
			na = oip->num_attached;
			arr[offset] = oip->opcode;
			put_unaligned_be16(oip->sa, arr + offset + 2);
			if (rctd)
				arr[offset + 5] |= 0x2;
			if (FF_SA & oip->flags)
				arr[offset + 5] |= 0x1;
			put_unaligned_be16(oip->len_mask[0], arr + offset + 6);
			if (rctd)
				put_unaligned_be16(0xa, arr + offset + 8);
			r_oip = oip;
			for (k = 0, oip = oip->arrp; k < na; ++k, ++oip) {
				if (F_INV_OP & oip->flags)
					continue;
				offset += bump;
				arr[offset] = oip->opcode;
				put_unaligned_be16(oip->sa, arr + offset + 2);
				if (rctd)
					arr[offset + 5] |= 0x2;
				if (FF_SA & oip->flags)
					arr[offset + 5] |= 0x1;
				put_unaligned_be16(oip->len_mask[0],
						   arr + offset + 6);
				if (rctd)
					put_unaligned_be16(0xa,
							   arr + offset + 8);
			}
			oip = r_oip;
			offset += bump;
		}
		break;
	case 1:	/* one command: opcode only */
	case 2:	/* one command: opcode plus service action */
	case 3:	/* one command: if sa==0 then opcode only else opcode+sa */
		sdeb_i = opcode_ind_arr[req_opcode];
		oip = &opcode_info_arr[sdeb_i];
		if (F_INV_OP & oip->flags) {
			supp = 1;
			offset = 4;
		} else {
			if (1 == reporting_opts) {
				if (FF_SA & oip->flags) {
					mk_sense_invalid_fld(scp, SDEB_IN_CDB,
							     2, 2);
					kfree(arr);
					return check_condition_result;
				}
				req_sa = 0;
			} else if (2 == reporting_opts &&
				   0 == (FF_SA & oip->flags)) {
				mk_sense_invalid_fld(scp, SDEB_IN_CDB, 4, -1);
				kfree(arr);	/* point at requested sa */
				return check_condition_result;
			}
			if (0 == (FF_SA & oip->flags) &&
			    req_opcode == oip->opcode)
				supp = 3;
			else if (0 == (FF_SA & oip->flags)) {
				na = oip->num_attached;
				for (k = 0, oip = oip->arrp; k < na;
				     ++k, ++oip) {
					if (req_opcode == oip->opcode)
						break;
				}
				supp = (k >= na) ? 1 : 3;
			} else if (req_sa != oip->sa) {
				na = oip->num_attached;
				for (k = 0, oip = oip->arrp; k < na;
				     ++k, ++oip) {
					if (req_sa == oip->sa)
						break;
				}
				supp = (k >= na) ? 1 : 3;
			} else
				supp = 3;
			if (3 == supp) {
				u = oip->len_mask[0];
				put_unaligned_be16(u, arr + 2);
				arr[4] = oip->opcode;
				for (k = 1; k < u; ++k)
					arr[4 + k] = (k < 16) ?
						 oip->len_mask[k] : 0xff;
				offset = 4 + u;
			} else
				offset = 4;
		}
		arr[1] = (rctd ? 0x80 : 0) | supp;
		if (rctd) {
			put_unaligned_be16(0xa, arr + offset);
			offset += 12;
		}
		break;
	default:
		mk_sense_invalid_fld(scp, SDEB_IN_CDB, 2, 2);
		kfree(arr);
		return check_condition_result;
	}
	offset = (offset < a_len) ? offset : a_len;
	len = (offset < alloc_len) ? offset : alloc_len;
	errsts = fill_from_dev_buffer(scp, arr, len);
	kfree(arr);
	return errsts;
}

static int resp_rsup_tmfs(struct scsi_cmnd *scp,
			  struct sdebug_dev_info *devip)
{
	bool repd;
	u32 alloc_len, len;
	u8 arr[16];
	u8 *cmd = scp->cmnd;

	memset(arr, 0, sizeof(arr));
	repd = !!(cmd[2] & 0x80);
	alloc_len = get_unaligned_be32(cmd + 6);
	if (alloc_len < 4) {
		mk_sense_invalid_fld(scp, SDEB_IN_CDB, 6, -1);
		return check_condition_result;
	}
	arr[0] = 0xc8;		/* ATS | ATSS | LURS */
	arr[1] = 0x1;		/* ITNRS */
	if (repd) {
		arr[3] = 0xc;
		len = 16;
	} else
		len = 4;

	len = (len < alloc_len) ? len : alloc_len;
	return fill_from_dev_buffer(scp, arr, len);
}

/* <<Following mode page info copied from ST318451LW>> */

static int resp_err_recov_pg(unsigned char *p, int pcontrol, int target)
{	/* Read-Write Error Recovery page for mode_sense */
	unsigned char err_recov_pg[] = {0x1, 0xa, 0xc0, 11, 240, 0, 0, 0,
					5, 0, 0xff, 0xff};

	memcpy(p, err_recov_pg, sizeof(err_recov_pg));
	if (1 == pcontrol)
		memset(p + 2, 0, sizeof(err_recov_pg) - 2);
	return sizeof(err_recov_pg);
}

static int resp_disconnect_pg(unsigned char *p, int pcontrol, int target)
{ 	/* Disconnect-Reconnect page for mode_sense */
	unsigned char disconnect_pg[] = {0x2, 0xe, 128, 128, 0, 10, 0, 0,
					 0, 0, 0, 0, 0, 0, 0, 0};

	memcpy(p, disconnect_pg, sizeof(disconnect_pg));
	if (1 == pcontrol)
		memset(p + 2, 0, sizeof(disconnect_pg) - 2);
	return sizeof(disconnect_pg);
}

static int resp_format_pg(unsigned char *p, int pcontrol, int target)
{       /* Format device page for mode_sense */
	unsigned char format_pg[] = {0x3, 0x16, 0, 0, 0, 0, 0, 0,
				     0, 0, 0, 0, 0, 0, 0, 0,
				     0, 0, 0, 0, 0x40, 0, 0, 0};

	memcpy(p, format_pg, sizeof(format_pg));
	put_unaligned_be16(sdebug_sectors_per, p + 10);
	put_unaligned_be16(sdebug_sector_size, p + 12);
	if (sdebug_removable)
		p[20] |= 0x20; /* should agree with INQUIRY */
	if (1 == pcontrol)
		memset(p + 2, 0, sizeof(format_pg) - 2);
	return sizeof(format_pg);
}

static unsigned char caching_pg[] = {0x8, 18, 0x14, 0, 0xff, 0xff, 0, 0,
				     0xff, 0xff, 0xff, 0xff, 0x80, 0x14, 0, 0,
				     0, 0, 0, 0};

static int resp_caching_pg(unsigned char *p, int pcontrol, int target)
{ 	/* Caching page for mode_sense */
	unsigned char ch_caching_pg[] = {/* 0x8, 18, */ 0x4, 0, 0, 0, 0, 0,
		0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
	unsigned char d_caching_pg[] = {0x8, 18, 0x14, 0, 0xff, 0xff, 0, 0,
		0xff, 0xff, 0xff, 0xff, 0x80, 0x14, 0, 0,     0, 0, 0, 0};

	if (SDEBUG_OPT_N_WCE & sdebug_opts)
		caching_pg[2] &= ~0x4;	/* set WCE=0 (default WCE=1) */
	memcpy(p, caching_pg, sizeof(caching_pg));
	if (1 == pcontrol)
		memcpy(p + 2, ch_caching_pg, sizeof(ch_caching_pg));
	else if (2 == pcontrol)
		memcpy(p, d_caching_pg, sizeof(d_caching_pg));
	return sizeof(caching_pg);
}

static unsigned char ctrl_m_pg[] = {0xa, 10, 2, 0, 0, 0, 0, 0,
				    0, 0, 0x2, 0x4b};

static int resp_ctrl_m_pg(unsigned char *p, int pcontrol, int target)
{ 	/* Control mode page for mode_sense */
	unsigned char ch_ctrl_m_pg[] = {/* 0xa, 10, */ 0x6, 0, 0, 0, 0, 0,
					0, 0, 0, 0};
	unsigned char d_ctrl_m_pg[] = {0xa, 10, 2, 0, 0, 0, 0, 0,
				     0, 0, 0x2, 0x4b};

	if (sdebug_dsense)
		ctrl_m_pg[2] |= 0x4;
	else
		ctrl_m_pg[2] &= ~0x4;

	if (sdebug_ato)
		ctrl_m_pg[5] |= 0x80; /* ATO=1 */

	memcpy(p, ctrl_m_pg, sizeof(ctrl_m_pg));
	if (1 == pcontrol)
		memcpy(p + 2, ch_ctrl_m_pg, sizeof(ch_ctrl_m_pg));
	else if (2 == pcontrol)
		memcpy(p, d_ctrl_m_pg, sizeof(d_ctrl_m_pg));
	return sizeof(ctrl_m_pg);
}

/* IO Advice Hints Grouping mode page */
static int resp_grouping_m_pg(unsigned char *p, int pcontrol, int target)
{
	/* IO Advice Hints Grouping mode page */
	struct grouping_m_pg {
		u8 page_code;	/* OR 0x40 when subpage_code > 0 */
		u8 subpage_code;
		__be16 page_length;
		u8 reserved[12];
		struct scsi_io_group_descriptor descr[MAXIMUM_NUMBER_OF_STREAMS];
	};
	static const struct grouping_m_pg gr_m_pg = {
		.page_code = 0xa | 0x40,
		.subpage_code = 5,
		.page_length = cpu_to_be16(sizeof(gr_m_pg) - 4),
		.descr = {
			{ .st_enble = 1 },
			{ .st_enble = 1 },
			{ .st_enble = 1 },
			{ .st_enble = 1 },
			{ .st_enble = 1 },
			{ .st_enble = 0 },
		}
	};

	BUILD_BUG_ON(sizeof(struct grouping_m_pg) !=
		     16 + MAXIMUM_NUMBER_OF_STREAMS * 16);
	memcpy(p, &gr_m_pg, sizeof(gr_m_pg));
	if (1 == pcontrol) {
		/* There are no changeable values so clear from byte 4 on. */
		memset(p + 4, 0, sizeof(gr_m_pg) - 4);
	}
	return sizeof(gr_m_pg);
}

static int resp_iec_m_pg(unsigned char *p, int pcontrol, int target)
{	/* Informational Exceptions control mode page for mode_sense */
	unsigned char ch_iec_m_pg[] = {/* 0x1c, 0xa, */ 0x4, 0xf, 0, 0, 0, 0,
				       0, 0, 0x0, 0x0};
	unsigned char d_iec_m_pg[] = {0x1c, 0xa, 0x08, 0, 0, 0, 0, 0,
				      0, 0, 0x0, 0x0};

	memcpy(p, iec_m_pg, sizeof(iec_m_pg));
	if (1 == pcontrol)
		memcpy(p + 2, ch_iec_m_pg, sizeof(ch_iec_m_pg));
	else if (2 == pcontrol)
		memcpy(p, d_iec_m_pg, sizeof(d_iec_m_pg));
	return sizeof(iec_m_pg);
}

static int resp_sas_sf_m_pg(unsigned char *p, int pcontrol, int target)
{	/* SAS SSP mode page - short format for mode_sense */
	unsigned char sas_sf_m_pg[] = {0x19, 0x6,
		0x6, 0x0, 0x7, 0xd0, 0x0, 0x0};

	memcpy(p, sas_sf_m_pg, sizeof(sas_sf_m_pg));
	if (1 == pcontrol)
		memset(p + 2, 0, sizeof(sas_sf_m_pg) - 2);
	return sizeof(sas_sf_m_pg);
}


static int resp_sas_pcd_m_spg(unsigned char *p, int pcontrol, int target,
			      int target_dev_id)
{	/* SAS phy control and discover mode page for mode_sense */
	unsigned char sas_pcd_m_pg[] = {0x59, 0x1, 0, 0x64, 0, 0x6, 0, 2,
		    0, 0, 0, 0, 0x10, 0x9, 0x8, 0x0,
		    0, 0, 0, 0, 0, 0, 0, 0,	/* insert SAS addr */
		    0, 0, 0, 0, 0, 0, 0, 0,	/* insert SAS addr */
		    0x2, 0, 0, 0, 0, 0, 0, 0,
		    0x88, 0x99, 0, 0, 0, 0, 0, 0,
		    0, 0, 0, 0, 0, 0, 0, 0,
		    0, 1, 0, 0, 0x10, 0x9, 0x8, 0x0,
		    0, 0, 0, 0, 0, 0, 0, 0,	/* insert SAS addr */
		    0, 0, 0, 0, 0, 0, 0, 0,	/* insert SAS addr */
		    0x3, 0, 0, 0, 0, 0, 0, 0,
		    0x88, 0x99, 0, 0, 0, 0, 0, 0,
		    0, 0, 0, 0, 0, 0, 0, 0,
		};
	int port_a, port_b;

	put_unaligned_be64(naa3_comp_a, sas_pcd_m_pg + 16);
	put_unaligned_be64(naa3_comp_c + 1, sas_pcd_m_pg + 24);
	put_unaligned_be64(naa3_comp_a, sas_pcd_m_pg + 64);
	put_unaligned_be64(naa3_comp_c + 1, sas_pcd_m_pg + 72);
	port_a = target_dev_id + 1;
	port_b = port_a + 1;
	memcpy(p, sas_pcd_m_pg, sizeof(sas_pcd_m_pg));
	put_unaligned_be32(port_a, p + 20);
	put_unaligned_be32(port_b, p + 48 + 20);
	if (1 == pcontrol)
		memset(p + 4, 0, sizeof(sas_pcd_m_pg) - 4);
	return sizeof(sas_pcd_m_pg);
}

static int resp_sas_sha_m_spg(unsigned char *p, int pcontrol)
{	/* SAS SSP shared protocol specific port mode subpage */
	unsigned char sas_sha_m_pg[] = {0x59, 0x2, 0, 0xc, 0, 0x6, 0x10, 0,
		    0, 0, 0, 0, 0, 0, 0, 0,
		};

	memcpy(p, sas_sha_m_pg, sizeof(sas_sha_m_pg));
	if (1 == pcontrol)
		memset(p + 4, 0, sizeof(sas_sha_m_pg) - 4);
	return sizeof(sas_sha_m_pg);
}

/* PAGE_SIZE is more than necessary but provides room for future expansion. */
#define SDEBUG_MAX_MSENSE_SZ PAGE_SIZE

static int resp_mode_sense(struct scsi_cmnd *scp,
			   struct sdebug_dev_info *devip)
{
	int pcontrol, pcode, subpcode, bd_len;
	unsigned char dev_spec;
	u32 alloc_len, offset, len;
	int target_dev_id;
	int target = scp->device->id;
	unsigned char *ap;
	unsigned char *arr __free(kfree);
	unsigned char *cmd = scp->cmnd;
	bool dbd, llbaa, msense_6, is_disk, is_zbc;

	arr = kzalloc(SDEBUG_MAX_MSENSE_SZ, GFP_ATOMIC);
	if (!arr)
		return -ENOMEM;
	dbd = !!(cmd[1] & 0x8);		/* disable block descriptors */
	pcontrol = (cmd[2] & 0xc0) >> 6;
	pcode = cmd[2] & 0x3f;
	subpcode = cmd[3];
	msense_6 = (MODE_SENSE == cmd[0]);
	llbaa = msense_6 ? false : !!(cmd[1] & 0x10);
	is_disk = (sdebug_ptype == TYPE_DISK);
	is_zbc = devip->zoned;
	if ((is_disk || is_zbc) && !dbd)
		bd_len = llbaa ? 16 : 8;
	else
		bd_len = 0;
	alloc_len = msense_6 ? cmd[4] : get_unaligned_be16(cmd + 7);
	if (0x3 == pcontrol) {  /* Saving values not supported */
		mk_sense_buffer(scp, ILLEGAL_REQUEST, SAVING_PARAMS_UNSUP, 0);
		return check_condition_result;
	}
	target_dev_id = ((devip->sdbg_host->shost->host_no + 1) * 2000) +
			(devip->target * 1000) - 3;
	/* for disks+zbc set DPOFUA bit and clear write protect (WP) bit */
	if (is_disk || is_zbc) {
		dev_spec = 0x10;	/* =0x90 if WP=1 implies read-only */
		if (sdebug_wp)
			dev_spec |= 0x80;
	} else
		dev_spec = 0x0;
	if (msense_6) {
		arr[2] = dev_spec;
		arr[3] = bd_len;
		offset = 4;
	} else {
		arr[3] = dev_spec;
		if (16 == bd_len)
			arr[4] = 0x1;	/* set LONGLBA bit */
		arr[7] = bd_len;	/* assume 255 or less */
		offset = 8;
	}
	ap = arr + offset;
	if ((bd_len > 0) && (!sdebug_capacity))
		sdebug_capacity = get_sdebug_capacity();

	if (8 == bd_len) {
		if (sdebug_capacity > 0xfffffffe)
			put_unaligned_be32(0xffffffff, ap + 0);
		else
			put_unaligned_be32(sdebug_capacity, ap + 0);
		put_unaligned_be16(sdebug_sector_size, ap + 6);
		offset += bd_len;
		ap = arr + offset;
	} else if (16 == bd_len) {
		put_unaligned_be64((u64)sdebug_capacity, ap + 0);
		put_unaligned_be32(sdebug_sector_size, ap + 12);
		offset += bd_len;
		ap = arr + offset;
	}

	/*
	 * N.B. If len>0 before resp_*_pg() call, then form of that call should be:
	 *        len += resp_*_pg(ap + len, pcontrol, target);
	 */
	switch (pcode) {
	case 0x1:	/* Read-Write error recovery page, direct access */
		if (subpcode > 0x0 && subpcode < 0xff)
			goto bad_subpcode;
		len = resp_err_recov_pg(ap, pcontrol, target);
		offset += len;
		break;
	case 0x2:	/* Disconnect-Reconnect page, all devices */
		if (subpcode > 0x0 && subpcode < 0xff)
			goto bad_subpcode;
		len = resp_disconnect_pg(ap, pcontrol, target);
		offset += len;
		break;
	case 0x3:       /* Format device page, direct access */
		if (subpcode > 0x0 && subpcode < 0xff)
			goto bad_subpcode;
		if (is_disk) {
			len = resp_format_pg(ap, pcontrol, target);
			offset += len;
		} else {
			goto bad_pcode;
		}
		break;
	case 0x8:	/* Caching page, direct access */
		if (subpcode > 0x0 && subpcode < 0xff)
			goto bad_subpcode;
		if (is_disk || is_zbc) {
			len = resp_caching_pg(ap, pcontrol, target);
			offset += len;
		} else {
			goto bad_pcode;
		}
		break;
	case 0xa:	/* Control Mode page, all devices */
		switch (subpcode) {
		case 0:
			len = resp_ctrl_m_pg(ap, pcontrol, target);
			break;
		case 0x05:
			len = resp_grouping_m_pg(ap, pcontrol, target);
			break;
		case 0xff:
			len = resp_ctrl_m_pg(ap, pcontrol, target);
			len += resp_grouping_m_pg(ap + len, pcontrol, target);
			break;
		default:
			goto bad_subpcode;
		}
		offset += len;
		break;
	case 0x19:	/* if spc==1 then sas phy, control+discover */
		if (subpcode > 0x2 && subpcode < 0xff)
			goto bad_subpcode;
		len = 0;
		if ((0x0 == subpcode) || (0xff == subpcode))
			len += resp_sas_sf_m_pg(ap + len, pcontrol, target);
		if ((0x1 == subpcode) || (0xff == subpcode))
			len += resp_sas_pcd_m_spg(ap + len, pcontrol, target,
						  target_dev_id);
		if ((0x2 == subpcode) || (0xff == subpcode))
			len += resp_sas_sha_m_spg(ap + len, pcontrol);
		offset += len;
		break;
	case 0x1c:	/* Informational Exceptions Mode page, all devices */
		if (subpcode > 0x0 && subpcode < 0xff)
			goto bad_subpcode;
		len = resp_iec_m_pg(ap, pcontrol, target);
		offset += len;
		break;
	case 0x3f:	/* Read all Mode pages */
		if (subpcode > 0x0 && subpcode < 0xff)
			goto bad_subpcode;
		len = resp_err_recov_pg(ap, pcontrol, target);
		len += resp_disconnect_pg(ap + len, pcontrol, target);
		if (is_disk) {
			len += resp_format_pg(ap + len, pcontrol, target);
			len += resp_caching_pg(ap + len, pcontrol, target);
		} else if (is_zbc) {
			len += resp_caching_pg(ap + len, pcontrol, target);
		}
		len += resp_ctrl_m_pg(ap + len, pcontrol, target);
		if (0xff == subpcode)
			len += resp_grouping_m_pg(ap + len, pcontrol, target);
		len += resp_sas_sf_m_pg(ap + len, pcontrol, target);
		if (0xff == subpcode) {
			len += resp_sas_pcd_m_spg(ap + len, pcontrol, target,
						  target_dev_id);
			len += resp_sas_sha_m_spg(ap + len, pcontrol);
		}
		len += resp_iec_m_pg(ap + len, pcontrol, target);
		offset += len;
		break;
	default:
		goto bad_pcode;
	}
	if (msense_6)
		arr[0] = offset - 1;
	else
		put_unaligned_be16((offset - 2), arr + 0);
	return fill_from_dev_buffer(scp, arr, min_t(u32, alloc_len, offset));

bad_pcode:
	mk_sense_invalid_fld(scp, SDEB_IN_CDB, 2, 5);
	return check_condition_result;

bad_subpcode:
	mk_sense_invalid_fld(scp, SDEB_IN_CDB, 3, -1);
	return check_condition_result;
}

#define SDEBUG_MAX_MSELECT_SZ 512

static int resp_mode_select(struct scsi_cmnd *scp,
			    struct sdebug_dev_info *devip)
{
	int pf, sp, ps, md_len, bd_len, off, spf, pg_len;
	int param_len, res, mpage;
	unsigned char arr[SDEBUG_MAX_MSELECT_SZ];
	unsigned char *cmd = scp->cmnd;
	int mselect6 = (MODE_SELECT == cmd[0]);

	memset(arr, 0, sizeof(arr));
	pf = cmd[1] & 0x10;
	sp = cmd[1] & 0x1;
	param_len = mselect6 ? cmd[4] : get_unaligned_be16(cmd + 7);
	if ((0 == pf) || sp || (param_len > SDEBUG_MAX_MSELECT_SZ)) {
		mk_sense_invalid_fld(scp, SDEB_IN_CDB, mselect6 ? 4 : 7, -1);
		return check_condition_result;
	}
	res = fetch_to_dev_buffer(scp, arr, param_len);
	if (-1 == res)
		return DID_ERROR << 16;
	else if (sdebug_verbose && (res < param_len))
		sdev_printk(KERN_INFO, scp->device,
			    "%s: cdb indicated=%d, IO sent=%d bytes\n",
			    __func__, param_len, res);
	md_len = mselect6 ? (arr[0] + 1) : (get_unaligned_be16(arr + 0) + 2);
	bd_len = mselect6 ? arr[3] : get_unaligned_be16(arr + 6);
	off = bd_len + (mselect6 ? 4 : 8);
	if (md_len > 2 || off >= res) {
		mk_sense_invalid_fld(scp, SDEB_IN_DATA, 0, -1);
		return check_condition_result;
	}
	mpage = arr[off] & 0x3f;
	ps = !!(arr[off] & 0x80);
	if (ps) {
		mk_sense_invalid_fld(scp, SDEB_IN_DATA, off, 7);
		return check_condition_result;
	}
	spf = !!(arr[off] & 0x40);
	pg_len = spf ? (get_unaligned_be16(arr + off + 2) + 4) :
		       (arr[off + 1] + 2);
	if ((pg_len + off) > param_len) {
		mk_sense_buffer(scp, ILLEGAL_REQUEST,
				PARAMETER_LIST_LENGTH_ERR, 0);
		return check_condition_result;
	}
	switch (mpage) {
	case 0x8:      /* Caching Mode page */
		if (caching_pg[1] == arr[off + 1]) {
			memcpy(caching_pg + 2, arr + off + 2,
			       sizeof(caching_pg) - 2);
			goto set_mode_changed_ua;
		}
		break;
	case 0xa:      /* Control Mode page */
		if (ctrl_m_pg[1] == arr[off + 1]) {
			memcpy(ctrl_m_pg + 2, arr + off + 2,
			       sizeof(ctrl_m_pg) - 2);
			if (ctrl_m_pg[4] & 0x8)
				sdebug_wp = true;
			else
				sdebug_wp = false;
			sdebug_dsense = !!(ctrl_m_pg[2] & 0x4);
			goto set_mode_changed_ua;
		}
		break;
	case 0x1c:      /* Informational Exceptions Mode page */
		if (iec_m_pg[1] == arr[off + 1]) {
			memcpy(iec_m_pg + 2, arr + off + 2,
			       sizeof(iec_m_pg) - 2);
			goto set_mode_changed_ua;
		}
		break;
	default:
		break;
	}
	mk_sense_invalid_fld(scp, SDEB_IN_DATA, off, 5);
	return check_condition_result;
set_mode_changed_ua:
	set_bit(SDEBUG_UA_MODE_CHANGED, devip->uas_bm);
	return 0;
}

static int resp_temp_l_pg(unsigned char *arr)
{
	unsigned char temp_l_pg[] = {0x0, 0x0, 0x3, 0x2, 0x0, 38,
				     0x0, 0x1, 0x3, 0x2, 0x0, 65,
		};

	memcpy(arr, temp_l_pg, sizeof(temp_l_pg));
	return sizeof(temp_l_pg);
}

static int resp_ie_l_pg(unsigned char *arr)
{
	unsigned char ie_l_pg[] = {0x0, 0x0, 0x3, 0x3, 0x0, 0x0, 38,
		};

	memcpy(arr, ie_l_pg, sizeof(ie_l_pg));
	if (iec_m_pg[2] & 0x4) {	/* TEST bit set */
		arr[4] = THRESHOLD_EXCEEDED;
		arr[5] = 0xff;
	}
	return sizeof(ie_l_pg);
}

static int resp_env_rep_l_spg(unsigned char *arr)
{
	unsigned char env_rep_l_spg[] = {0x0, 0x0, 0x23, 0x8,
					 0x0, 40, 72, 0xff, 45, 18, 0, 0,
					 0x1, 0x0, 0x23, 0x8,
					 0x0, 55, 72, 35, 55, 45, 0, 0,
		};

	memcpy(arr, env_rep_l_spg, sizeof(env_rep_l_spg));
	return sizeof(env_rep_l_spg);
}

#define SDEBUG_MAX_LSENSE_SZ 512

static int resp_log_sense(struct scsi_cmnd *scp,
			  struct sdebug_dev_info *devip)
{
	int ppc, sp, pcode, subpcode;
	u32 alloc_len, len, n;
	unsigned char arr[SDEBUG_MAX_LSENSE_SZ];
	unsigned char *cmd = scp->cmnd;

	memset(arr, 0, sizeof(arr));
	ppc = cmd[1] & 0x2;
	sp = cmd[1] & 0x1;
	if (ppc || sp) {
		mk_sense_invalid_fld(scp, SDEB_IN_CDB, 1, ppc ? 1 : 0);
		return check_condition_result;
	}
	pcode = cmd[2] & 0x3f;
	subpcode = cmd[3] & 0xff;
	alloc_len = get_unaligned_be16(cmd + 7);
	arr[0] = pcode;
	if (0 == subpcode) {
		switch (pcode) {
		case 0x0:	/* Supported log pages log page */
			n = 4;
			arr[n++] = 0x0;		/* this page */
			arr[n++] = 0xd;		/* Temperature */
			arr[n++] = 0x2f;	/* Informational exceptions */
			arr[3] = n - 4;
			break;
		case 0xd:	/* Temperature log page */
			arr[3] = resp_temp_l_pg(arr + 4);
			break;
		case 0x2f:	/* Informational exceptions log page */
			arr[3] = resp_ie_l_pg(arr + 4);
			break;
		default:
			mk_sense_invalid_fld(scp, SDEB_IN_CDB, 2, 5);
			return check_condition_result;
		}
	} else if (0xff == subpcode) {
		arr[0] |= 0x40;
		arr[1] = subpcode;
		switch (pcode) {
		case 0x0:	/* Supported log pages and subpages log page */
			n = 4;
			arr[n++] = 0x0;
			arr[n++] = 0x0;		/* 0,0 page */
			arr[n++] = 0x0;
			arr[n++] = 0xff;	/* this page */
			arr[n++] = 0xd;
			arr[n++] = 0x0;		/* Temperature */
			arr[n++] = 0xd;
			arr[n++] = 0x1;		/* Environment reporting */
			arr[n++] = 0xd;
			arr[n++] = 0xff;	/* all 0xd subpages */
			arr[n++] = 0x2f;
			arr[n++] = 0x0;	/* Informational exceptions */
			arr[n++] = 0x2f;
			arr[n++] = 0xff;	/* all 0x2f subpages */
			arr[3] = n - 4;
			break;
		case 0xd:	/* Temperature subpages */
			n = 4;
			arr[n++] = 0xd;
			arr[n++] = 0x0;		/* Temperature */
			arr[n++] = 0xd;
			arr[n++] = 0x1;		/* Environment reporting */
			arr[n++] = 0xd;
			arr[n++] = 0xff;	/* these subpages */
			arr[3] = n - 4;
			break;
		case 0x2f:	/* Informational exceptions subpages */
			n = 4;
			arr[n++] = 0x2f;
			arr[n++] = 0x0;		/* Informational exceptions */
			arr[n++] = 0x2f;
			arr[n++] = 0xff;	/* these subpages */
			arr[3] = n - 4;
			break;
		default:
			mk_sense_invalid_fld(scp, SDEB_IN_CDB, 2, 5);
			return check_condition_result;
		}
	} else if (subpcode > 0) {
		arr[0] |= 0x40;
		arr[1] = subpcode;
		if (pcode == 0xd && subpcode == 1)
			arr[3] = resp_env_rep_l_spg(arr + 4);
		else {
			mk_sense_invalid_fld(scp, SDEB_IN_CDB, 2, 5);
			return check_condition_result;
		}
	} else {
		mk_sense_invalid_fld(scp, SDEB_IN_CDB, 3, -1);
		return check_condition_result;
	}
	len = min_t(u32, get_unaligned_be16(arr + 2) + 4, alloc_len);
	return fill_from_dev_buffer(scp, arr,
		    min_t(u32, len, SDEBUG_MAX_INQ_ARR_SZ));
}

static inline bool sdebug_dev_is_zoned(struct sdebug_dev_info *devip)
{
	return devip->nr_zones != 0;
}

static struct sdeb_zone_state *zbc_zone(struct sdebug_dev_info *devip,
					unsigned long long lba)
{
	u32 zno = lba >> devip->zsize_shift;
	struct sdeb_zone_state *zsp;

	if (devip->zcap == devip->zsize || zno < devip->nr_conv_zones)
		return &devip->zstate[zno];

	/*
	 * If the zone capacity is less than the zone size, adjust for gap
	 * zones.
	 */
	zno = 2 * zno - devip->nr_conv_zones;
	WARN_ONCE(zno >= devip->nr_zones, "%u > %u\n", zno, devip->nr_zones);
	zsp = &devip->zstate[zno];
	if (lba >= zsp->z_start + zsp->z_size)
		zsp++;
	WARN_ON_ONCE(lba >= zsp->z_start + zsp->z_size);
	return zsp;
}

static inline bool zbc_zone_is_conv(struct sdeb_zone_state *zsp)
{
	return zsp->z_type == ZBC_ZTYPE_CNV;
}

static inline bool zbc_zone_is_gap(struct sdeb_zone_state *zsp)
{
	return zsp->z_type == ZBC_ZTYPE_GAP;
}

static inline bool zbc_zone_is_seq(struct sdeb_zone_state *zsp)
{
	return !zbc_zone_is_conv(zsp) && !zbc_zone_is_gap(zsp);
}

static void zbc_close_zone(struct sdebug_dev_info *devip,
			   struct sdeb_zone_state *zsp)
{
	enum sdebug_z_cond zc;

	if (!zbc_zone_is_seq(zsp))
		return;

	zc = zsp->z_cond;
	if (!(zc == ZC2_IMPLICIT_OPEN || zc == ZC3_EXPLICIT_OPEN))
		return;

	if (zc == ZC2_IMPLICIT_OPEN)
		devip->nr_imp_open--;
	else
		devip->nr_exp_open--;

	if (zsp->z_wp == zsp->z_start) {
		zsp->z_cond = ZC1_EMPTY;
	} else {
		zsp->z_cond = ZC4_CLOSED;
		devip->nr_closed++;
	}
}

static void zbc_close_imp_open_zone(struct sdebug_dev_info *devip)
{
	struct sdeb_zone_state *zsp = &devip->zstate[0];
	unsigned int i;

	for (i = 0; i < devip->nr_zones; i++, zsp++) {
		if (zsp->z_cond == ZC2_IMPLICIT_OPEN) {
			zbc_close_zone(devip, zsp);
			return;
		}
	}
}

static void zbc_open_zone(struct sdebug_dev_info *devip,
			  struct sdeb_zone_state *zsp, bool explicit)
{
	enum sdebug_z_cond zc;

	if (!zbc_zone_is_seq(zsp))
		return;

	zc = zsp->z_cond;
	if ((explicit && zc == ZC3_EXPLICIT_OPEN) ||
	    (!explicit && zc == ZC2_IMPLICIT_OPEN))
		return;

	/* Close an implicit open zone if necessary */
	if (explicit && zsp->z_cond == ZC2_IMPLICIT_OPEN)
		zbc_close_zone(devip, zsp);
	else if (devip->max_open &&
		 devip->nr_imp_open + devip->nr_exp_open >= devip->max_open)
		zbc_close_imp_open_zone(devip);

	if (zsp->z_cond == ZC4_CLOSED)
		devip->nr_closed--;
	if (explicit) {
		zsp->z_cond = ZC3_EXPLICIT_OPEN;
		devip->nr_exp_open++;
	} else {
		zsp->z_cond = ZC2_IMPLICIT_OPEN;
		devip->nr_imp_open++;
	}
}

static inline void zbc_set_zone_full(struct sdebug_dev_info *devip,
				     struct sdeb_zone_state *zsp)
{
	switch (zsp->z_cond) {
	case ZC2_IMPLICIT_OPEN:
		devip->nr_imp_open--;
		break;
	case ZC3_EXPLICIT_OPEN:
		devip->nr_exp_open--;
		break;
	default:
		WARN_ONCE(true, "Invalid zone %llu condition %x\n",
			  zsp->z_start, zsp->z_cond);
		break;
	}
	zsp->z_cond = ZC5_FULL;
}

static void zbc_inc_wp(struct sdebug_dev_info *devip,
		       unsigned long long lba, unsigned int num)
{
	struct sdeb_zone_state *zsp = zbc_zone(devip, lba);
	unsigned long long n, end, zend = zsp->z_start + zsp->z_size;

	if (!zbc_zone_is_seq(zsp))
		return;

	if (zsp->z_type == ZBC_ZTYPE_SWR) {
		zsp->z_wp += num;
		if (zsp->z_wp >= zend)
			zbc_set_zone_full(devip, zsp);
		return;
	}

	while (num) {
		if (lba != zsp->z_wp)
			zsp->z_non_seq_resource = true;

		end = lba + num;
		if (end >= zend) {
			n = zend - lba;
			zsp->z_wp = zend;
		} else if (end > zsp->z_wp) {
			n = num;
			zsp->z_wp = end;
		} else {
			n = num;
		}
		if (zsp->z_wp >= zend)
			zbc_set_zone_full(devip, zsp);

		num -= n;
		lba += n;
		if (num) {
			zsp++;
			zend = zsp->z_start + zsp->z_size;
		}
	}
}

static int check_zbc_access_params(struct scsi_cmnd *scp,
			unsigned long long lba, unsigned int num, bool write)
{
	struct scsi_device *sdp = scp->device;
	struct sdebug_dev_info *devip = (struct sdebug_dev_info *)sdp->hostdata;
	struct sdeb_zone_state *zsp = zbc_zone(devip, lba);
	struct sdeb_zone_state *zsp_end = zbc_zone(devip, lba + num - 1);

	if (!write) {
		/* For host-managed, reads cannot cross zone types boundaries */
		if (zsp->z_type != zsp_end->z_type) {
			mk_sense_buffer(scp, ILLEGAL_REQUEST,
					LBA_OUT_OF_RANGE,
					READ_INVDATA_ASCQ);
			return check_condition_result;
		}
		return 0;
	}

	/* Writing into a gap zone is not allowed */
	if (zbc_zone_is_gap(zsp)) {
		mk_sense_buffer(scp, ILLEGAL_REQUEST, LBA_OUT_OF_RANGE,
				ATTEMPT_ACCESS_GAP);
		return check_condition_result;
	}

	/* No restrictions for writes within conventional zones */
	if (zbc_zone_is_conv(zsp)) {
		if (!zbc_zone_is_conv(zsp_end)) {
			mk_sense_buffer(scp, ILLEGAL_REQUEST,
					LBA_OUT_OF_RANGE,
					WRITE_BOUNDARY_ASCQ);
			return check_condition_result;
		}
		return 0;
	}

	if (zsp->z_type == ZBC_ZTYPE_SWR) {
		/* Writes cannot cross sequential zone boundaries */
		if (zsp_end != zsp) {
			mk_sense_buffer(scp, ILLEGAL_REQUEST,
					LBA_OUT_OF_RANGE,
					WRITE_BOUNDARY_ASCQ);
			return check_condition_result;
		}
		/* Cannot write full zones */
		if (zsp->z_cond == ZC5_FULL) {
			mk_sense_buffer(scp, ILLEGAL_REQUEST,
					INVALID_FIELD_IN_CDB, 0);
			return check_condition_result;
		}
		/* Writes must be aligned to the zone WP */
		if (lba != zsp->z_wp) {
			mk_sense_buffer(scp, ILLEGAL_REQUEST,
					LBA_OUT_OF_RANGE,
					UNALIGNED_WRITE_ASCQ);
			return check_condition_result;
		}
	}

	/* Handle implicit open of closed and empty zones */
	if (zsp->z_cond == ZC1_EMPTY || zsp->z_cond == ZC4_CLOSED) {
		if (devip->max_open &&
		    devip->nr_exp_open >= devip->max_open) {
			mk_sense_buffer(scp, DATA_PROTECT,
					INSUFF_RES_ASC,
					INSUFF_ZONE_ASCQ);
			return check_condition_result;
		}
		zbc_open_zone(devip, zsp, false);
	}

	return 0;
}

static inline int check_device_access_params
			(struct scsi_cmnd *scp, unsigned long long lba,
			 unsigned int num, bool write)
{
	struct scsi_device *sdp = scp->device;
	struct sdebug_dev_info *devip = (struct sdebug_dev_info *)sdp->hostdata;

	if (lba + num > sdebug_capacity) {
		mk_sense_buffer(scp, ILLEGAL_REQUEST, LBA_OUT_OF_RANGE, 0);
		return check_condition_result;
	}
	/* transfer length excessive (tie in to block limits VPD page) */
	if (num > sdebug_store_sectors) {
		/* needs work to find which cdb byte 'num' comes from */
		mk_sense_buffer(scp, ILLEGAL_REQUEST, INVALID_FIELD_IN_CDB, 0);
		return check_condition_result;
	}
	if (write && unlikely(sdebug_wp)) {
		mk_sense_buffer(scp, DATA_PROTECT, WRITE_PROTECTED, 0x2);
		return check_condition_result;
	}
	if (sdebug_dev_is_zoned(devip))
		return check_zbc_access_params(scp, lba, num, write);

	return 0;
}

/*
 * Note: if BUG_ON() fires it usually indicates a problem with the parser
 * tables. Perhaps a missing F_FAKE_RW or FF_MEDIA_IO flag. Response functions
 * that access any of the "stores" in struct sdeb_store_info should call this
 * function with bug_if_fake_rw set to true.
 */
static inline struct sdeb_store_info *devip2sip(struct sdebug_dev_info *devip,
						bool bug_if_fake_rw)
{
	if (sdebug_fake_rw) {
		BUG_ON(bug_if_fake_rw);	/* See note above */
		return NULL;
	}
	return xa_load(per_store_ap, devip->sdbg_host->si_idx);
}

static inline void
sdeb_read_lock(rwlock_t *lock)
{
	if (sdebug_no_rwlock)
		__acquire(lock);
	else
		read_lock(lock);
}

static inline void
sdeb_read_unlock(rwlock_t *lock)
{
	if (sdebug_no_rwlock)
		__release(lock);
	else
		read_unlock(lock);
}

static inline void
sdeb_write_lock(rwlock_t *lock)
{
	if (sdebug_no_rwlock)
		__acquire(lock);
	else
		write_lock(lock);
}

static inline void
sdeb_write_unlock(rwlock_t *lock)
{
	if (sdebug_no_rwlock)
		__release(lock);
	else
		write_unlock(lock);
}

static inline void
sdeb_data_read_lock(struct sdeb_store_info *sip)
{
	BUG_ON(!sip);

	sdeb_read_lock(&sip->macc_data_lck);
}

static inline void
sdeb_data_read_unlock(struct sdeb_store_info *sip)
{
	BUG_ON(!sip);

	sdeb_read_unlock(&sip->macc_data_lck);
}

static inline void
sdeb_data_write_lock(struct sdeb_store_info *sip)
{
	BUG_ON(!sip);

	sdeb_write_lock(&sip->macc_data_lck);
}

static inline void
sdeb_data_write_unlock(struct sdeb_store_info *sip)
{
	BUG_ON(!sip);

	sdeb_write_unlock(&sip->macc_data_lck);
}

static inline void
sdeb_data_sector_read_lock(struct sdeb_store_info *sip)
{
	BUG_ON(!sip);

	sdeb_read_lock(&sip->macc_sector_lck);
}

static inline void
sdeb_data_sector_read_unlock(struct sdeb_store_info *sip)
{
	BUG_ON(!sip);

	sdeb_read_unlock(&sip->macc_sector_lck);
}

static inline void
sdeb_data_sector_write_lock(struct sdeb_store_info *sip)
{
	BUG_ON(!sip);

	sdeb_write_lock(&sip->macc_sector_lck);
}

static inline void
sdeb_data_sector_write_unlock(struct sdeb_store_info *sip)
{
	BUG_ON(!sip);

	sdeb_write_unlock(&sip->macc_sector_lck);
}

/*
 * Atomic locking:
 * We simplify the atomic model to allow only 1x atomic write and many non-
 * atomic reads or writes for all LBAs.

 * A RW lock has a similar bahaviour:
 * Only 1x writer and many readers.

 * So use a RW lock for per-device read and write locking:
 * An atomic access grabs the lock as a writer and non-atomic grabs the lock
 * as a reader.
 */

static inline void
sdeb_data_lock(struct sdeb_store_info *sip, bool atomic)
{
	if (atomic)
		sdeb_data_write_lock(sip);
	else
		sdeb_data_read_lock(sip);
}

static inline void
sdeb_data_unlock(struct sdeb_store_info *sip, bool atomic)
{
	if (atomic)
		sdeb_data_write_unlock(sip);
	else
		sdeb_data_read_unlock(sip);
}

/* Allow many reads but only 1x write per sector */
static inline void
sdeb_data_sector_lock(struct sdeb_store_info *sip, bool do_write)
{
	if (do_write)
		sdeb_data_sector_write_lock(sip);
	else
		sdeb_data_sector_read_lock(sip);
}

static inline void
sdeb_data_sector_unlock(struct sdeb_store_info *sip, bool do_write)
{
	if (do_write)
		sdeb_data_sector_write_unlock(sip);
	else
		sdeb_data_sector_read_unlock(sip);
}

static inline void
sdeb_meta_read_lock(struct sdeb_store_info *sip)
{
	if (sdebug_no_rwlock) {
		if (sip)
			__acquire(&sip->macc_meta_lck);
		else
			__acquire(&sdeb_fake_rw_lck);
	} else {
		if (sip)
			read_lock(&sip->macc_meta_lck);
		else
			read_lock(&sdeb_fake_rw_lck);
	}
}

static inline void
sdeb_meta_read_unlock(struct sdeb_store_info *sip)
{
	if (sdebug_no_rwlock) {
		if (sip)
			__release(&sip->macc_meta_lck);
		else
			__release(&sdeb_fake_rw_lck);
	} else {
		if (sip)
			read_unlock(&sip->macc_meta_lck);
		else
			read_unlock(&sdeb_fake_rw_lck);
	}
}

static inline void
sdeb_meta_write_lock(struct sdeb_store_info *sip)
{
	if (sdebug_no_rwlock) {
		if (sip)
			__acquire(&sip->macc_meta_lck);
		else
			__acquire(&sdeb_fake_rw_lck);
	} else {
		if (sip)
			write_lock(&sip->macc_meta_lck);
		else
			write_lock(&sdeb_fake_rw_lck);
	}
}

static inline void
sdeb_meta_write_unlock(struct sdeb_store_info *sip)
{
	if (sdebug_no_rwlock) {
		if (sip)
			__release(&sip->macc_meta_lck);
		else
			__release(&sdeb_fake_rw_lck);
	} else {
		if (sip)
			write_unlock(&sip->macc_meta_lck);
		else
			write_unlock(&sdeb_fake_rw_lck);
	}
}

/* Returns number of bytes copied or -1 if error. */
static int do_device_access(struct sdeb_store_info *sip, struct scsi_cmnd *scp,
			    u32 sg_skip, u64 lba, u32 num, u8 group_number,
			    bool do_write, bool atomic)
{
	int ret;
	u64 block;
	enum dma_data_direction dir;
	struct scsi_data_buffer *sdb = &scp->sdb;
	u8 *fsp;
	int i, total = 0;

	/*
	 * Even though reads are inherently atomic (in this driver), we expect
	 * the atomic flag only for writes.
	 */
	if (!do_write && atomic)
		return -1;

	if (do_write) {
		dir = DMA_TO_DEVICE;
		write_since_sync = true;
	} else {
		dir = DMA_FROM_DEVICE;
	}

	if (!sdb->length || !sip)
		return 0;
	if (scp->sc_data_direction != dir)
		return -1;

	if (do_write && group_number < ARRAY_SIZE(writes_by_group_number))
		atomic_long_inc(&writes_by_group_number[group_number]);

	fsp = sip->storep;

	block = do_div(lba, sdebug_store_sectors);

	/* Only allow 1x atomic write or multiple non-atomic writes at any given time */
	sdeb_data_lock(sip, atomic);
	for (i = 0; i < num; i++) {
		/* We shouldn't need to lock for atomic writes, but do it anyway */
		sdeb_data_sector_lock(sip, do_write);
		ret = sg_copy_buffer(sdb->table.sgl, sdb->table.nents,
		   fsp + (block * sdebug_sector_size),
		   sdebug_sector_size, sg_skip, do_write);
		sdeb_data_sector_unlock(sip, do_write);
		total += ret;
		if (ret != sdebug_sector_size)
			break;
		sg_skip += sdebug_sector_size;
		if (++block >= sdebug_store_sectors)
			block = 0;
	}
	sdeb_data_unlock(sip, atomic);

	return total;
}

/* Returns number of bytes copied or -1 if error. */
static int do_dout_fetch(struct scsi_cmnd *scp, u32 num, u8 *doutp)
{
	struct scsi_data_buffer *sdb = &scp->sdb;

	if (!sdb->length)
		return 0;
	if (scp->sc_data_direction != DMA_TO_DEVICE)
		return -1;
	return sg_copy_buffer(sdb->table.sgl, sdb->table.nents, doutp,
			      num * sdebug_sector_size, 0, true);
}

/* If sip->storep+lba compares equal to arr(num), then copy top half of
 * arr into sip->storep+lba and return true. If comparison fails then
 * return false. */
static bool comp_write_worker(struct sdeb_store_info *sip, u64 lba, u32 num,
			      const u8 *arr, bool compare_only)
{
	bool res;
	u64 block, rest = 0;
	u32 store_blks = sdebug_store_sectors;
	u32 lb_size = sdebug_sector_size;
	u8 *fsp = sip->storep;

	block = do_div(lba, store_blks);
	if (block + num > store_blks)
		rest = block + num - store_blks;

	res = !memcmp(fsp + (block * lb_size), arr, (num - rest) * lb_size);
	if (!res)
		return res;
	if (rest)
		res = memcmp(fsp, arr + ((num - rest) * lb_size),
			     rest * lb_size);
	if (!res)
		return res;
	if (compare_only)
		return true;
	arr += num * lb_size;
	memcpy(fsp + (block * lb_size), arr, (num - rest) * lb_size);
	if (rest)
		memcpy(fsp, arr + ((num - rest) * lb_size), rest * lb_size);
	return res;
}

static __be16 dif_compute_csum(const void *buf, int len)
{
	__be16 csum;

	if (sdebug_guard)
		csum = (__force __be16)ip_compute_csum(buf, len);
	else
		csum = cpu_to_be16(crc_t10dif(buf, len));

	return csum;
}

static int dif_verify(struct t10_pi_tuple *sdt, const void *data,
		      sector_t sector, u32 ei_lba)
{
	__be16 csum = dif_compute_csum(data, sdebug_sector_size);

	if (sdt->guard_tag != csum) {
		pr_err("GUARD check failed on sector %lu rcvd 0x%04x, data 0x%04x\n",
			(unsigned long)sector,
			be16_to_cpu(sdt->guard_tag),
			be16_to_cpu(csum));
		return 0x01;
	}
	if (sdebug_dif == T10_PI_TYPE1_PROTECTION &&
	    be32_to_cpu(sdt->ref_tag) != (sector & 0xffffffff)) {
		pr_err("REF check failed on sector %lu\n",
			(unsigned long)sector);
		return 0x03;
	}
	if (sdebug_dif == T10_PI_TYPE2_PROTECTION &&
	    be32_to_cpu(sdt->ref_tag) != ei_lba) {
		pr_err("REF check failed on sector %lu\n",
			(unsigned long)sector);
		return 0x03;
	}
	return 0;
}

static void dif_copy_prot(struct scsi_cmnd *scp, sector_t sector,
			  unsigned int sectors, bool read)
{
	size_t resid;
	void *paddr;
	struct sdeb_store_info *sip = devip2sip((struct sdebug_dev_info *)
						scp->device->hostdata, true);
	struct t10_pi_tuple *dif_storep = sip->dif_storep;
	const void *dif_store_end = dif_storep + sdebug_store_sectors;
	struct sg_mapping_iter miter;

	/* Bytes of protection data to copy into sgl */
	resid = sectors * sizeof(*dif_storep);

	sg_miter_start(&miter, scsi_prot_sglist(scp),
		       scsi_prot_sg_count(scp), SG_MITER_ATOMIC |
		       (read ? SG_MITER_TO_SG : SG_MITER_FROM_SG));

	while (sg_miter_next(&miter) && resid > 0) {
		size_t len = min_t(size_t, miter.length, resid);
		void *start = dif_store(sip, sector);
		size_t rest = 0;

		if (dif_store_end < start + len)
			rest = start + len - dif_store_end;

		paddr = miter.addr;

		if (read)
			memcpy(paddr, start, len - rest);
		else
			memcpy(start, paddr, len - rest);

		if (rest) {
			if (read)
				memcpy(paddr + len - rest, dif_storep, rest);
			else
				memcpy(dif_storep, paddr + len - rest, rest);
		}

		sector += len / sizeof(*dif_storep);
		resid -= len;
	}
	sg_miter_stop(&miter);
}

static int prot_verify_read(struct scsi_cmnd *scp, sector_t start_sec,
			    unsigned int sectors, u32 ei_lba)
{
	int ret = 0;
	unsigned int i;
	sector_t sector;
	struct sdeb_store_info *sip = devip2sip((struct sdebug_dev_info *)
						scp->device->hostdata, true);
	struct t10_pi_tuple *sdt;

	for (i = 0; i < sectors; i++, ei_lba++) {
		sector = start_sec + i;
		sdt = dif_store(sip, sector);

		if (sdt->app_tag == cpu_to_be16(0xffff))
			continue;

		/*
		 * Because scsi_debug acts as both initiator and
		 * target we proceed to verify the PI even if
		 * RDPROTECT=3. This is done so the "initiator" knows
		 * which type of error to return. Otherwise we would
		 * have to iterate over the PI twice.
		 */
		if (scp->cmnd[1] >> 5) { /* RDPROTECT */
			ret = dif_verify(sdt, lba2fake_store(sip, sector),
					 sector, ei_lba);
			if (ret) {
				dif_errors++;
				break;
			}
		}
	}

	dif_copy_prot(scp, start_sec, sectors, true);
	dix_reads++;

	return ret;
}

static int resp_read_dt0(struct scsi_cmnd *scp, struct sdebug_dev_info *devip)
{
	bool check_prot;
	u32 num;
	u32 ei_lba;
	int ret;
	u64 lba;
	struct sdeb_store_info *sip = devip2sip(devip, true);
	u8 *cmd = scp->cmnd;
	bool meta_data_locked = false;

	switch (cmd[0]) {
	case READ_16:
		ei_lba = 0;
		lba = get_unaligned_be64(cmd + 2);
		num = get_unaligned_be32(cmd + 10);
		check_prot = true;
		break;
	case READ_10:
		ei_lba = 0;
		lba = get_unaligned_be32(cmd + 2);
		num = get_unaligned_be16(cmd + 7);
		check_prot = true;
		break;
	case READ_6:
		ei_lba = 0;
		lba = (u32)cmd[3] | (u32)cmd[2] << 8 |
		      (u32)(cmd[1] & 0x1f) << 16;
		num = (0 == cmd[4]) ? 256 : cmd[4];
		check_prot = true;
		break;
	case READ_12:
		ei_lba = 0;
		lba = get_unaligned_be32(cmd + 2);
		num = get_unaligned_be32(cmd + 6);
		check_prot = true;
		break;
	case XDWRITEREAD_10:
		ei_lba = 0;
		lba = get_unaligned_be32(cmd + 2);
		num = get_unaligned_be16(cmd + 7);
		check_prot = false;
		break;
	default:	/* assume READ(32) */
		lba = get_unaligned_be64(cmd + 12);
		ei_lba = get_unaligned_be32(cmd + 20);
		num = get_unaligned_be32(cmd + 28);
		check_prot = false;
		break;
	}
	if (unlikely(have_dif_prot && check_prot)) {
		if (sdebug_dif == T10_PI_TYPE2_PROTECTION &&
		    (cmd[1] & 0xe0)) {
			mk_sense_invalid_opcode(scp);
			return check_condition_result;
		}
		if ((sdebug_dif == T10_PI_TYPE1_PROTECTION ||
		     sdebug_dif == T10_PI_TYPE3_PROTECTION) &&
		    (cmd[1] & 0xe0) == 0)
			sdev_printk(KERN_ERR, scp->device, "Unprotected RD "
				    "to DIF device\n");
	}
	if (unlikely((sdebug_opts & SDEBUG_OPT_SHORT_TRANSFER) &&
		     atomic_read(&sdeb_inject_pending))) {
		num /= 2;
		atomic_set(&sdeb_inject_pending, 0);
	}

	/*
	 * When checking device access params, for reads we only check data
	 * versus what is set at init time, so no need to lock.
	 */
	ret = check_device_access_params(scp, lba, num, false);
	if (ret)
		return ret;
	if (unlikely((SDEBUG_OPT_MEDIUM_ERR & sdebug_opts) &&
		     (lba <= (sdebug_medium_error_start + sdebug_medium_error_count - 1)) &&
		     ((lba + num) > sdebug_medium_error_start))) {
		/* claim unrecoverable read error */
		mk_sense_buffer(scp, MEDIUM_ERROR, UNRECOVERED_READ_ERR, 0);
		/* set info field and valid bit for fixed descriptor */
		if (0x70 == (scp->sense_buffer[0] & 0x7f)) {
			scp->sense_buffer[0] |= 0x80;	/* Valid bit */
			ret = (lba < OPT_MEDIUM_ERR_ADDR)
			      ? OPT_MEDIUM_ERR_ADDR : (int)lba;
			put_unaligned_be32(ret, scp->sense_buffer + 3);
		}
		scsi_set_resid(scp, scsi_bufflen(scp));
		return check_condition_result;
	}

	if (sdebug_dev_is_zoned(devip) ||
	    (sdebug_dix && scsi_prot_sg_count(scp)))  {
		sdeb_meta_read_lock(sip);
		meta_data_locked = true;
	}

	/* DIX + T10 DIF */
	if (unlikely(sdebug_dix && scsi_prot_sg_count(scp))) {
		switch (prot_verify_read(scp, lba, num, ei_lba)) {
		case 1: /* Guard tag error */
			if (cmd[1] >> 5 != 3) { /* RDPROTECT != 3 */
				sdeb_meta_read_unlock(sip);
				mk_sense_buffer(scp, ABORTED_COMMAND, 0x10, 1);
				return check_condition_result;
			} else if (scp->prot_flags & SCSI_PROT_GUARD_CHECK) {
				sdeb_meta_read_unlock(sip);
				mk_sense_buffer(scp, ILLEGAL_REQUEST, 0x10, 1);
				return illegal_condition_result;
			}
			break;
		case 3: /* Reference tag error */
			if (cmd[1] >> 5 != 3) { /* RDPROTECT != 3 */
				sdeb_meta_read_unlock(sip);
				mk_sense_buffer(scp, ABORTED_COMMAND, 0x10, 3);
				return check_condition_result;
			} else if (scp->prot_flags & SCSI_PROT_REF_CHECK) {
				sdeb_meta_read_unlock(sip);
				mk_sense_buffer(scp, ILLEGAL_REQUEST, 0x10, 3);
				return illegal_condition_result;
			}
			break;
		}
	}

	ret = do_device_access(sip, scp, 0, lba, num, 0, false, false);
	if (meta_data_locked)
		sdeb_meta_read_unlock(sip);
	if (unlikely(ret == -1))
		return DID_ERROR << 16;

	scsi_set_resid(scp, scsi_bufflen(scp) - ret);

	if (unlikely((sdebug_opts & SDEBUG_OPT_RECOV_DIF_DIX) &&
		     atomic_read(&sdeb_inject_pending))) {
		if (sdebug_opts & SDEBUG_OPT_RECOVERED_ERR) {
			mk_sense_buffer(scp, RECOVERED_ERROR, THRESHOLD_EXCEEDED, 0);
			atomic_set(&sdeb_inject_pending, 0);
			return check_condition_result;
		} else if (sdebug_opts & SDEBUG_OPT_DIF_ERR) {
			/* Logical block guard check failed */
			mk_sense_buffer(scp, ABORTED_COMMAND, 0x10, 1);
			atomic_set(&sdeb_inject_pending, 0);
			return illegal_condition_result;
		} else if (SDEBUG_OPT_DIX_ERR & sdebug_opts) {
			mk_sense_buffer(scp, ILLEGAL_REQUEST, 0x10, 1);
			atomic_set(&sdeb_inject_pending, 0);
			return illegal_condition_result;
		}
	}
	return 0;
}

static int prot_verify_write(struct scsi_cmnd *SCpnt, sector_t start_sec,
			     unsigned int sectors, u32 ei_lba)
{
	int ret;
	struct t10_pi_tuple *sdt;
	void *daddr;
	sector_t sector = start_sec;
	int ppage_offset;
	int dpage_offset;
	struct sg_mapping_iter diter;
	struct sg_mapping_iter piter;

	BUG_ON(scsi_sg_count(SCpnt) == 0);
	BUG_ON(scsi_prot_sg_count(SCpnt) == 0);

	sg_miter_start(&piter, scsi_prot_sglist(SCpnt),
			scsi_prot_sg_count(SCpnt),
			SG_MITER_ATOMIC | SG_MITER_FROM_SG);
	sg_miter_start(&diter, scsi_sglist(SCpnt), scsi_sg_count(SCpnt),
			SG_MITER_ATOMIC | SG_MITER_FROM_SG);

	/* For each protection page */
	while (sg_miter_next(&piter)) {
		dpage_offset = 0;
		if (WARN_ON(!sg_miter_next(&diter))) {
			ret = 0x01;
			goto out;
		}

		for (ppage_offset = 0; ppage_offset < piter.length;
		     ppage_offset += sizeof(struct t10_pi_tuple)) {
			/* If we're at the end of the current
			 * data page advance to the next one
			 */
			if (dpage_offset >= diter.length) {
				if (WARN_ON(!sg_miter_next(&diter))) {
					ret = 0x01;
					goto out;
				}
				dpage_offset = 0;
			}

			sdt = piter.addr + ppage_offset;
			daddr = diter.addr + dpage_offset;

			if (SCpnt->cmnd[1] >> 5 != 3) { /* WRPROTECT */
				ret = dif_verify(sdt, daddr, sector, ei_lba);
				if (ret)
					goto out;
			}

			sector++;
			ei_lba++;
			dpage_offset += sdebug_sector_size;
		}
		diter.consumed = dpage_offset;
		sg_miter_stop(&diter);
	}
	sg_miter_stop(&piter);

	dif_copy_prot(SCpnt, start_sec, sectors, false);
	dix_writes++;

	return 0;

out:
	dif_errors++;
	sg_miter_stop(&diter);
	sg_miter_stop(&piter);
	return ret;
}

static unsigned long lba_to_map_index(sector_t lba)
{
	if (sdebug_unmap_alignment)
		lba += sdebug_unmap_granularity - sdebug_unmap_alignment;
	sector_div(lba, sdebug_unmap_granularity);
	return lba;
}

static sector_t map_index_to_lba(unsigned long index)
{
	sector_t lba = index * sdebug_unmap_granularity;

	if (sdebug_unmap_alignment)
		lba -= sdebug_unmap_granularity - sdebug_unmap_alignment;
	return lba;
}

static unsigned int map_state(struct sdeb_store_info *sip, sector_t lba,
			      unsigned int *num)
{
	sector_t end;
	unsigned int mapped;
	unsigned long index;
	unsigned long next;

	index = lba_to_map_index(lba);
	mapped = test_bit(index, sip->map_storep);

	if (mapped)
		next = find_next_zero_bit(sip->map_storep, map_size, index);
	else
		next = find_next_bit(sip->map_storep, map_size, index);

	end = min_t(sector_t, sdebug_store_sectors,  map_index_to_lba(next));
	*num = end - lba;
	return mapped;
}

static void map_region(struct sdeb_store_info *sip, sector_t lba,
		       unsigned int len)
{
	sector_t end = lba + len;

	while (lba < end) {
		unsigned long index = lba_to_map_index(lba);

		if (index < map_size)
			set_bit(index, sip->map_storep);

		lba = map_index_to_lba(index + 1);
	}
}

static void unmap_region(struct sdeb_store_info *sip, sector_t lba,
			 unsigned int len)
{
	sector_t end = lba + len;
	u8 *fsp = sip->storep;

	while (lba < end) {
		unsigned long index = lba_to_map_index(lba);

		if (lba == map_index_to_lba(index) &&
		    lba + sdebug_unmap_granularity <= end &&
		    index < map_size) {
			clear_bit(index, sip->map_storep);
			if (sdebug_lbprz) {  /* for LBPRZ=2 return 0xff_s */
				memset(fsp + lba * sdebug_sector_size,
				       (sdebug_lbprz & 1) ? 0 : 0xff,
				       sdebug_sector_size *
				       sdebug_unmap_granularity);
			}
			if (sip->dif_storep) {
				memset(sip->dif_storep + lba, 0xff,
				       sizeof(*sip->dif_storep) *
				       sdebug_unmap_granularity);
			}
		}
		lba = map_index_to_lba(index + 1);
	}
}

static int resp_write_dt0(struct scsi_cmnd *scp, struct sdebug_dev_info *devip)
{
	bool check_prot;
	u32 num;
	u8 group = 0;
	u32 ei_lba;
	int ret;
	u64 lba;
	struct sdeb_store_info *sip = devip2sip(devip, true);
	u8 *cmd = scp->cmnd;
	bool meta_data_locked = false;

	switch (cmd[0]) {
	case WRITE_16:
		ei_lba = 0;
		lba = get_unaligned_be64(cmd + 2);
		num = get_unaligned_be32(cmd + 10);
		group = cmd[14] & 0x3f;
		check_prot = true;
		break;
	case WRITE_10:
		ei_lba = 0;
		lba = get_unaligned_be32(cmd + 2);
		group = cmd[6] & 0x3f;
		num = get_unaligned_be16(cmd + 7);
		check_prot = true;
		break;
	case WRITE_6:
		ei_lba = 0;
		lba = (u32)cmd[3] | (u32)cmd[2] << 8 |
		      (u32)(cmd[1] & 0x1f) << 16;
		num = (0 == cmd[4]) ? 256 : cmd[4];
		check_prot = true;
		break;
	case WRITE_12:
		ei_lba = 0;
		lba = get_unaligned_be32(cmd + 2);
		num = get_unaligned_be32(cmd + 6);
		group = cmd[6] & 0x3f;
		check_prot = true;
		break;
	case 0x53:	/* XDWRITEREAD(10) */
		ei_lba = 0;
		lba = get_unaligned_be32(cmd + 2);
		group = cmd[6] & 0x1f;
		num = get_unaligned_be16(cmd + 7);
		check_prot = false;
		break;
	default:	/* assume WRITE(32) */
		group = cmd[6] & 0x3f;
		lba = get_unaligned_be64(cmd + 12);
		ei_lba = get_unaligned_be32(cmd + 20);
		num = get_unaligned_be32(cmd + 28);
		check_prot = false;
		break;
	}
	if (unlikely(have_dif_prot && check_prot)) {
		if (sdebug_dif == T10_PI_TYPE2_PROTECTION &&
		    (cmd[1] & 0xe0)) {
			mk_sense_invalid_opcode(scp);
			return check_condition_result;
		}
		if ((sdebug_dif == T10_PI_TYPE1_PROTECTION ||
		     sdebug_dif == T10_PI_TYPE3_PROTECTION) &&
		    (cmd[1] & 0xe0) == 0)
			sdev_printk(KERN_ERR, scp->device, "Unprotected WR "
				    "to DIF device\n");
	}

	if (sdebug_dev_is_zoned(devip) ||
	    (sdebug_dix && scsi_prot_sg_count(scp)) ||
	    scsi_debug_lbp())  {
		sdeb_meta_write_lock(sip);
		meta_data_locked = true;
	}

	ret = check_device_access_params(scp, lba, num, true);
	if (ret) {
		if (meta_data_locked)
			sdeb_meta_write_unlock(sip);
		return ret;
	}

	/* DIX + T10 DIF */
	if (unlikely(sdebug_dix && scsi_prot_sg_count(scp))) {
		switch (prot_verify_write(scp, lba, num, ei_lba)) {
		case 1: /* Guard tag error */
			if (scp->prot_flags & SCSI_PROT_GUARD_CHECK) {
				sdeb_meta_write_unlock(sip);
				mk_sense_buffer(scp, ILLEGAL_REQUEST, 0x10, 1);
				return illegal_condition_result;
			} else if (scp->cmnd[1] >> 5 != 3) { /* WRPROTECT != 3 */
				sdeb_meta_write_unlock(sip);
				mk_sense_buffer(scp, ABORTED_COMMAND, 0x10, 1);
				return check_condition_result;
			}
			break;
		case 3: /* Reference tag error */
			if (scp->prot_flags & SCSI_PROT_REF_CHECK) {
				sdeb_meta_write_unlock(sip);
				mk_sense_buffer(scp, ILLEGAL_REQUEST, 0x10, 3);
				return illegal_condition_result;
			} else if (scp->cmnd[1] >> 5 != 3) { /* WRPROTECT != 3 */
				sdeb_meta_write_unlock(sip);
				mk_sense_buffer(scp, ABORTED_COMMAND, 0x10, 3);
				return check_condition_result;
			}
			break;
		}
	}

	ret = do_device_access(sip, scp, 0, lba, num, group, true, false);
	if (unlikely(scsi_debug_lbp()))
		map_region(sip, lba, num);

	/* If ZBC zone then bump its write pointer */
	if (sdebug_dev_is_zoned(devip))
		zbc_inc_wp(devip, lba, num);
	if (meta_data_locked)
		sdeb_meta_write_unlock(sip);

	if (unlikely(-1 == ret))
		return DID_ERROR << 16;
	else if (unlikely(sdebug_verbose &&
			  (ret < (num * sdebug_sector_size))))
		sdev_printk(KERN_INFO, scp->device,
			    "%s: write: cdb indicated=%u, IO sent=%d bytes\n",
			    my_name, num * sdebug_sector_size, ret);

	if (unlikely((sdebug_opts & SDEBUG_OPT_RECOV_DIF_DIX) &&
		     atomic_read(&sdeb_inject_pending))) {
		if (sdebug_opts & SDEBUG_OPT_RECOVERED_ERR) {
			mk_sense_buffer(scp, RECOVERED_ERROR, THRESHOLD_EXCEEDED, 0);
			atomic_set(&sdeb_inject_pending, 0);
			return check_condition_result;
		} else if (sdebug_opts & SDEBUG_OPT_DIF_ERR) {
			/* Logical block guard check failed */
			mk_sense_buffer(scp, ABORTED_COMMAND, 0x10, 1);
			atomic_set(&sdeb_inject_pending, 0);
			return illegal_condition_result;
		} else if (sdebug_opts & SDEBUG_OPT_DIX_ERR) {
			mk_sense_buffer(scp, ILLEGAL_REQUEST, 0x10, 1);
			atomic_set(&sdeb_inject_pending, 0);
			return illegal_condition_result;
		}
	}
	return 0;
}

/*
 * T10 has only specified WRITE SCATTERED(16) and WRITE SCATTERED(32).
 * No READ GATHERED yet (requires bidi or long cdb holding gather list).
 */
static int resp_write_scat(struct scsi_cmnd *scp,
			   struct sdebug_dev_info *devip)
{
	u8 *cmd = scp->cmnd;
	u8 *lrdp = NULL;
	u8 *up;
	struct sdeb_store_info *sip = devip2sip(devip, true);
	u8 wrprotect;
	u16 lbdof, num_lrd, k;
	u32 num, num_by, bt_len, lbdof_blen, sg_off, cum_lb;
	u32 lb_size = sdebug_sector_size;
	u32 ei_lba;
	u64 lba;
	u8 group;
	int ret, res;
	bool is_16;
	static const u32 lrd_size = 32; /* + parameter list header size */

	if (cmd[0] == VARIABLE_LENGTH_CMD) {
		is_16 = false;
		group = cmd[6] & 0x3f;
		wrprotect = (cmd[10] >> 5) & 0x7;
		lbdof = get_unaligned_be16(cmd + 12);
		num_lrd = get_unaligned_be16(cmd + 16);
		bt_len = get_unaligned_be32(cmd + 28);
	} else {        /* that leaves WRITE SCATTERED(16) */
		is_16 = true;
		wrprotect = (cmd[2] >> 5) & 0x7;
		lbdof = get_unaligned_be16(cmd + 4);
		num_lrd = get_unaligned_be16(cmd + 8);
		bt_len = get_unaligned_be32(cmd + 10);
		group = cmd[14] & 0x3f;
		if (unlikely(have_dif_prot)) {
			if (sdebug_dif == T10_PI_TYPE2_PROTECTION &&
			    wrprotect) {
				mk_sense_invalid_opcode(scp);
				return illegal_condition_result;
			}
			if ((sdebug_dif == T10_PI_TYPE1_PROTECTION ||
			     sdebug_dif == T10_PI_TYPE3_PROTECTION) &&
			     wrprotect == 0)
				sdev_printk(KERN_ERR, scp->device,
					    "Unprotected WR to DIF device\n");
		}
	}
	if ((num_lrd == 0) || (bt_len == 0))
		return 0;       /* T10 says these do-nothings are not errors */
	if (lbdof == 0) {
		if (sdebug_verbose)
			sdev_printk(KERN_INFO, scp->device,
				"%s: %s: LB Data Offset field bad\n",
				my_name, __func__);
		mk_sense_buffer(scp, ILLEGAL_REQUEST, INVALID_FIELD_IN_CDB, 0);
		return illegal_condition_result;
	}
	lbdof_blen = lbdof * lb_size;
	if ((lrd_size + (num_lrd * lrd_size)) > lbdof_blen) {
		if (sdebug_verbose)
			sdev_printk(KERN_INFO, scp->device,
				"%s: %s: LBA range descriptors don't fit\n",
				my_name, __func__);
		mk_sense_buffer(scp, ILLEGAL_REQUEST, INVALID_FIELD_IN_CDB, 0);
		return illegal_condition_result;
	}
	lrdp = kzalloc(lbdof_blen, GFP_ATOMIC | __GFP_NOWARN);
	if (lrdp == NULL)
		return SCSI_MLQUEUE_HOST_BUSY;
	if (sdebug_verbose)
		sdev_printk(KERN_INFO, scp->device,
			"%s: %s: Fetch header+scatter_list, lbdof_blen=%u\n",
			my_name, __func__, lbdof_blen);
	res = fetch_to_dev_buffer(scp, lrdp, lbdof_blen);
	if (res == -1) {
		ret = DID_ERROR << 16;
		goto err_out;
	}

	/* Just keep it simple and always lock for now */
	sdeb_meta_write_lock(sip);
	sg_off = lbdof_blen;
	/* Spec says Buffer xfer Length field in number of LBs in dout */
	cum_lb = 0;
	for (k = 0, up = lrdp + lrd_size; k < num_lrd; ++k, up += lrd_size) {
		lba = get_unaligned_be64(up + 0);
		num = get_unaligned_be32(up + 8);
		if (sdebug_verbose)
			sdev_printk(KERN_INFO, scp->device,
				"%s: %s: k=%d  LBA=0x%llx num=%u  sg_off=%u\n",
				my_name, __func__, k, lba, num, sg_off);
		if (num == 0)
			continue;
		ret = check_device_access_params(scp, lba, num, true);
		if (ret)
			goto err_out_unlock;
		num_by = num * lb_size;
		ei_lba = is_16 ? 0 : get_unaligned_be32(up + 12);

		if ((cum_lb + num) > bt_len) {
			if (sdebug_verbose)
				sdev_printk(KERN_INFO, scp->device,
				    "%s: %s: sum of blocks > data provided\n",
				    my_name, __func__);
			mk_sense_buffer(scp, ILLEGAL_REQUEST, WRITE_ERROR_ASC,
					0);
			ret = illegal_condition_result;
			goto err_out_unlock;
		}

		/* DIX + T10 DIF */
		if (unlikely(sdebug_dix && scsi_prot_sg_count(scp))) {
			int prot_ret = prot_verify_write(scp, lba, num,
							 ei_lba);

			if (prot_ret) {
				mk_sense_buffer(scp, ILLEGAL_REQUEST, 0x10,
						prot_ret);
				ret = illegal_condition_result;
				goto err_out_unlock;
			}
		}

		/*
		 * Write ranges atomically to keep as close to pre-atomic
		 * writes behaviour as possible.
		 */
		ret = do_device_access(sip, scp, sg_off, lba, num, group, true, true);
		/* If ZBC zone then bump its write pointer */
		if (sdebug_dev_is_zoned(devip))
			zbc_inc_wp(devip, lba, num);
		if (unlikely(scsi_debug_lbp()))
			map_region(sip, lba, num);
		if (unlikely(-1 == ret)) {
			ret = DID_ERROR << 16;
			goto err_out_unlock;
		} else if (unlikely(sdebug_verbose && (ret < num_by)))
			sdev_printk(KERN_INFO, scp->device,
			    "%s: write: cdb indicated=%u, IO sent=%d bytes\n",
			    my_name, num_by, ret);

		if (unlikely((sdebug_opts & SDEBUG_OPT_RECOV_DIF_DIX) &&
			     atomic_read(&sdeb_inject_pending))) {
			if (sdebug_opts & SDEBUG_OPT_RECOVERED_ERR) {
				mk_sense_buffer(scp, RECOVERED_ERROR, THRESHOLD_EXCEEDED, 0);
				atomic_set(&sdeb_inject_pending, 0);
				ret = check_condition_result;
				goto err_out_unlock;
			} else if (sdebug_opts & SDEBUG_OPT_DIF_ERR) {
				/* Logical block guard check failed */
				mk_sense_buffer(scp, ABORTED_COMMAND, 0x10, 1);
				atomic_set(&sdeb_inject_pending, 0);
				ret = illegal_condition_result;
				goto err_out_unlock;
			} else if (sdebug_opts & SDEBUG_OPT_DIX_ERR) {
				mk_sense_buffer(scp, ILLEGAL_REQUEST, 0x10, 1);
				atomic_set(&sdeb_inject_pending, 0);
				ret = illegal_condition_result;
				goto err_out_unlock;
			}
		}
		sg_off += num_by;
		cum_lb += num;
	}
	ret = 0;
err_out_unlock:
	sdeb_meta_write_unlock(sip);
err_out:
	kfree(lrdp);
	return ret;
}

static int resp_write_same(struct scsi_cmnd *scp, u64 lba, u32 num,
			   u32 ei_lba, bool unmap, bool ndob)
{
	struct scsi_device *sdp = scp->device;
	struct sdebug_dev_info *devip = (struct sdebug_dev_info *)sdp->hostdata;
	unsigned long long i;
	u64 block, lbaa;
	u32 lb_size = sdebug_sector_size;
	int ret;
	struct sdeb_store_info *sip = devip2sip((struct sdebug_dev_info *)
						scp->device->hostdata, true);
	u8 *fs1p;
	u8 *fsp;
	bool meta_data_locked = false;

	if (sdebug_dev_is_zoned(devip) || scsi_debug_lbp()) {
		sdeb_meta_write_lock(sip);
		meta_data_locked = true;
	}

	ret = check_device_access_params(scp, lba, num, true);
	if (ret)
		goto out;

	if (unmap && scsi_debug_lbp()) {
		unmap_region(sip, lba, num);
		goto out;
	}
	lbaa = lba;
	block = do_div(lbaa, sdebug_store_sectors);
	/* if ndob then zero 1 logical block, else fetch 1 logical block */
	fsp = sip->storep;
	fs1p = fsp + (block * lb_size);
	sdeb_data_write_lock(sip);
	if (ndob) {
		memset(fs1p, 0, lb_size);
		ret = 0;
	} else
		ret = fetch_to_dev_buffer(scp, fs1p, lb_size);

	if (-1 == ret) {
		ret = DID_ERROR << 16;
		goto out;
	} else if (sdebug_verbose && !ndob && (ret < lb_size))
		sdev_printk(KERN_INFO, scp->device,
			    "%s: %s: lb size=%u, IO sent=%d bytes\n",
			    my_name, "write same", lb_size, ret);

	/* Copy first sector to remaining blocks */
	for (i = 1 ; i < num ; i++) {
		lbaa = lba + i;
		block = do_div(lbaa, sdebug_store_sectors);
		memmove(fsp + (block * lb_size), fs1p, lb_size);
	}
	if (scsi_debug_lbp())
		map_region(sip, lba, num);
	/* If ZBC zone then bump its write pointer */
	if (sdebug_dev_is_zoned(devip))
		zbc_inc_wp(devip, lba, num);
	sdeb_data_write_unlock(sip);
	ret = 0;
out:
	if (meta_data_locked)
		sdeb_meta_write_unlock(sip);
	return ret;
}

static int resp_write_same_10(struct scsi_cmnd *scp,
			      struct sdebug_dev_info *devip)
{
	u8 *cmd = scp->cmnd;
	u32 lba;
	u16 num;
	u32 ei_lba = 0;
	bool unmap = false;

	if (cmd[1] & 0x8) {
		if (sdebug_lbpws10 == 0) {
			mk_sense_invalid_fld(scp, SDEB_IN_CDB, 1, 3);
			return check_condition_result;
		} else
			unmap = true;
	}
	lba = get_unaligned_be32(cmd + 2);
	num = get_unaligned_be16(cmd + 7);
	if (num > sdebug_write_same_length) {
		mk_sense_invalid_fld(scp, SDEB_IN_CDB, 7, -1);
		return check_condition_result;
	}
	return resp_write_same(scp, lba, num, ei_lba, unmap, false);
}

static int resp_write_same_16(struct scsi_cmnd *scp,
			      struct sdebug_dev_info *devip)
{
	u8 *cmd = scp->cmnd;
	u64 lba;
	u32 num;
	u32 ei_lba = 0;
	bool unmap = false;
	bool ndob = false;

	if (cmd[1] & 0x8) {	/* UNMAP */
		if (sdebug_lbpws == 0) {
			mk_sense_invalid_fld(scp, SDEB_IN_CDB, 1, 3);
			return check_condition_result;
		} else
			unmap = true;
	}
	if (cmd[1] & 0x1)  /* NDOB (no data-out buffer, assumes zeroes) */
		ndob = true;
	lba = get_unaligned_be64(cmd + 2);
	num = get_unaligned_be32(cmd + 10);
	if (num > sdebug_write_same_length) {
		mk_sense_invalid_fld(scp, SDEB_IN_CDB, 10, -1);
		return check_condition_result;
	}
	return resp_write_same(scp, lba, num, ei_lba, unmap, ndob);
}

/* Note the mode field is in the same position as the (lower) service action
 * field. For the Report supported operation codes command, SPC-4 suggests
 * each mode of this command should be reported separately; for future. */
static int resp_write_buffer(struct scsi_cmnd *scp,
			     struct sdebug_dev_info *devip)
{
	u8 *cmd = scp->cmnd;
	struct scsi_device *sdp = scp->device;
	struct sdebug_dev_info *dp;
	u8 mode;

	mode = cmd[1] & 0x1f;
	switch (mode) {
	case 0x4:	/* download microcode (MC) and activate (ACT) */
		/* set UAs on this device only */
		set_bit(SDEBUG_UA_BUS_RESET, devip->uas_bm);
		set_bit(SDEBUG_UA_MICROCODE_CHANGED, devip->uas_bm);
		break;
	case 0x5:	/* download MC, save and ACT */
		set_bit(SDEBUG_UA_MICROCODE_CHANGED_WO_RESET, devip->uas_bm);
		break;
	case 0x6:	/* download MC with offsets and ACT */
		/* set UAs on most devices (LUs) in this target */
		list_for_each_entry(dp,
				    &devip->sdbg_host->dev_info_list,
				    dev_list)
			if (dp->target == sdp->id) {
				set_bit(SDEBUG_UA_BUS_RESET, dp->uas_bm);
				if (devip != dp)
					set_bit(SDEBUG_UA_MICROCODE_CHANGED,
						dp->uas_bm);
			}
		break;
	case 0x7:	/* download MC with offsets, save, and ACT */
		/* set UA on all devices (LUs) in this target */
		list_for_each_entry(dp,
				    &devip->sdbg_host->dev_info_list,
				    dev_list)
			if (dp->target == sdp->id)
				set_bit(SDEBUG_UA_MICROCODE_CHANGED_WO_RESET,
					dp->uas_bm);
		break;
	default:
		/* do nothing for this command for other mode values */
		break;
	}
	return 0;
}

static int resp_comp_write(struct scsi_cmnd *scp,
			   struct sdebug_dev_info *devip)
{
	u8 *cmd = scp->cmnd;
	u8 *arr;
	struct sdeb_store_info *sip = devip2sip(devip, true);
	u64 lba;
	u32 dnum;
	u32 lb_size = sdebug_sector_size;
	u8 num;
	int ret;
	int retval = 0;

	lba = get_unaligned_be64(cmd + 2);
	num = cmd[13];		/* 1 to a maximum of 255 logical blocks */
	if (0 == num)
		return 0;	/* degenerate case, not an error */
	if (sdebug_dif == T10_PI_TYPE2_PROTECTION &&
	    (cmd[1] & 0xe0)) {
		mk_sense_invalid_opcode(scp);
		return check_condition_result;
	}
	if ((sdebug_dif == T10_PI_TYPE1_PROTECTION ||
	     sdebug_dif == T10_PI_TYPE3_PROTECTION) &&
	    (cmd[1] & 0xe0) == 0)
		sdev_printk(KERN_ERR, scp->device, "Unprotected WR "
			    "to DIF device\n");
	ret = check_device_access_params(scp, lba, num, false);
	if (ret)
		return ret;
	dnum = 2 * num;
	arr = kcalloc(lb_size, dnum, GFP_ATOMIC);
	if (NULL == arr) {
		mk_sense_buffer(scp, ILLEGAL_REQUEST, INSUFF_RES_ASC,
				INSUFF_RES_ASCQ);
		return check_condition_result;
	}

	ret = do_dout_fetch(scp, dnum, arr);
	if (ret == -1) {
		retval = DID_ERROR << 16;
		goto cleanup_free;
	} else if (sdebug_verbose && (ret < (dnum * lb_size)))
		sdev_printk(KERN_INFO, scp->device, "%s: compare_write: cdb "
			    "indicated=%u, IO sent=%d bytes\n", my_name,
			    dnum * lb_size, ret);

	sdeb_data_write_lock(sip);
	sdeb_meta_write_lock(sip);
	if (!comp_write_worker(sip, lba, num, arr, false)) {
		mk_sense_buffer(scp, MISCOMPARE, MISCOMPARE_VERIFY_ASC, 0);
		retval = check_condition_result;
		goto cleanup_unlock;
	}

	/* Cover sip->map_storep (which map_region()) sets with data lock */
	if (scsi_debug_lbp())
		map_region(sip, lba, num);
cleanup_unlock:
	sdeb_meta_write_unlock(sip);
	sdeb_data_write_unlock(sip);
cleanup_free:
	kfree(arr);
	return retval;
}

struct unmap_block_desc {
	__be64	lba;
	__be32	blocks;
	__be32	__reserved;
};

static int resp_unmap(struct scsi_cmnd *scp, struct sdebug_dev_info *devip)
{
	unsigned char *buf;
	struct unmap_block_desc *desc;
	struct sdeb_store_info *sip = devip2sip(devip, true);
	unsigned int i, payload_len, descriptors;
	int ret;

	if (!scsi_debug_lbp())
		return 0;	/* fib and say its done */
	payload_len = get_unaligned_be16(scp->cmnd + 7);
	BUG_ON(scsi_bufflen(scp) != payload_len);

	descriptors = (payload_len - 8) / 16;
	if (descriptors > sdebug_unmap_max_desc) {
		mk_sense_invalid_fld(scp, SDEB_IN_CDB, 7, -1);
		return check_condition_result;
	}

	buf = kzalloc(scsi_bufflen(scp), GFP_ATOMIC);
	if (!buf) {
		mk_sense_buffer(scp, ILLEGAL_REQUEST, INSUFF_RES_ASC,
				INSUFF_RES_ASCQ);
		return check_condition_result;
	}

	scsi_sg_copy_to_buffer(scp, buf, scsi_bufflen(scp));

	BUG_ON(get_unaligned_be16(&buf[0]) != payload_len - 2);
	BUG_ON(get_unaligned_be16(&buf[2]) != descriptors * 16);

	desc = (void *)&buf[8];

	sdeb_meta_write_lock(sip);

	for (i = 0 ; i < descriptors ; i++) {
		unsigned long long lba = get_unaligned_be64(&desc[i].lba);
		unsigned int num = get_unaligned_be32(&desc[i].blocks);

		ret = check_device_access_params(scp, lba, num, true);
		if (ret)
			goto out;

		unmap_region(sip, lba, num);
	}

	ret = 0;

out:
	sdeb_meta_write_unlock(sip);
	kfree(buf);

	return ret;
}

#define SDEBUG_GET_LBA_STATUS_LEN 32

static int resp_get_lba_status(struct scsi_cmnd *scp,
			       struct sdebug_dev_info *devip)
{
	u8 *cmd = scp->cmnd;
	u64 lba;
	u32 alloc_len, mapped, num;
	int ret;
	u8 arr[SDEBUG_GET_LBA_STATUS_LEN];

	lba = get_unaligned_be64(cmd + 2);
	alloc_len = get_unaligned_be32(cmd + 10);

	if (alloc_len < 24)
		return 0;

	ret = check_device_access_params(scp, lba, 1, false);
	if (ret)
		return ret;

	if (scsi_debug_lbp()) {
		struct sdeb_store_info *sip = devip2sip(devip, true);

		mapped = map_state(sip, lba, &num);
	} else {
		mapped = 1;
		/* following just in case virtual_gb changed */
		sdebug_capacity = get_sdebug_capacity();
		if (sdebug_capacity - lba <= 0xffffffff)
			num = sdebug_capacity - lba;
		else
			num = 0xffffffff;
	}

	memset(arr, 0, SDEBUG_GET_LBA_STATUS_LEN);
	put_unaligned_be32(20, arr);		/* Parameter Data Length */
	put_unaligned_be64(lba, arr + 8);	/* LBA */
	put_unaligned_be32(num, arr + 16);	/* Number of blocks */
	arr[20] = !mapped;		/* prov_stat=0: mapped; 1: dealloc */

	return fill_from_dev_buffer(scp, arr, SDEBUG_GET_LBA_STATUS_LEN);
}

static int resp_get_stream_status(struct scsi_cmnd *scp,
				  struct sdebug_dev_info *devip)
{
	u16 starting_stream_id, stream_id;
	const u8 *cmd = scp->cmnd;
	u32 alloc_len, offset;
	u8 arr[256] = {};
	struct scsi_stream_status_header *h = (void *)arr;

	starting_stream_id = get_unaligned_be16(cmd + 4);
	alloc_len = get_unaligned_be32(cmd + 10);

	if (alloc_len < 8) {
		mk_sense_invalid_fld(scp, SDEB_IN_CDB, 10, -1);
		return check_condition_result;
	}

	if (starting_stream_id >= MAXIMUM_NUMBER_OF_STREAMS) {
		mk_sense_invalid_fld(scp, SDEB_IN_CDB, 4, -1);
		return check_condition_result;
	}

	/*
	 * The GET STREAM STATUS command only reports status information
	 * about open streams. Treat the non-permanent stream as open.
	 */
	put_unaligned_be16(MAXIMUM_NUMBER_OF_STREAMS,
			   &h->number_of_open_streams);

	for (offset = 8, stream_id = starting_stream_id;
	     offset + 8 <= min_t(u32, alloc_len, sizeof(arr)) &&
		     stream_id < MAXIMUM_NUMBER_OF_STREAMS;
	     offset += 8, stream_id++) {
		struct scsi_stream_status *stream_status = (void *)arr + offset;

		stream_status->perm = stream_id < PERMANENT_STREAM_COUNT;
		put_unaligned_be16(stream_id,
				   &stream_status->stream_identifier);
		stream_status->rel_lifetime = stream_id + 1;
	}
	put_unaligned_be32(offset - 8, &h->len); /* PARAMETER DATA LENGTH */

	return fill_from_dev_buffer(scp, arr, min(offset, alloc_len));
}

static int resp_sync_cache(struct scsi_cmnd *scp,
			   struct sdebug_dev_info *devip)
{
	int res = 0;
	u64 lba;
	u32 num_blocks;
	u8 *cmd = scp->cmnd;

	if (cmd[0] == SYNCHRONIZE_CACHE) {	/* 10 byte cdb */
		lba = get_unaligned_be32(cmd + 2);
		num_blocks = get_unaligned_be16(cmd + 7);
	} else {				/* SYNCHRONIZE_CACHE(16) */
		lba = get_unaligned_be64(cmd + 2);
		num_blocks = get_unaligned_be32(cmd + 10);
	}
	if (lba + num_blocks > sdebug_capacity) {
		mk_sense_buffer(scp, ILLEGAL_REQUEST, LBA_OUT_OF_RANGE, 0);
		return check_condition_result;
	}
	if (!write_since_sync || (cmd[1] & 0x2))
		res = SDEG_RES_IMMED_MASK;
	else		/* delay if write_since_sync and IMMED clear */
		write_since_sync = false;
	return res;
}

/*
 * Assuming the LBA+num_blocks is not out-of-range, this function will return
 * CONDITION MET if the specified blocks will/have fitted in the cache, and
 * a GOOD status otherwise. Model a disk with a big cache and yield
 * CONDITION MET. Actually tries to bring range in main memory into the
 * cache associated with the CPU(s).
 */
static int resp_pre_fetch(struct scsi_cmnd *scp,
			  struct sdebug_dev_info *devip)
{
	int res = 0;
	u64 lba;
	u64 block, rest = 0;
	u32 nblks;
	u8 *cmd = scp->cmnd;
	struct sdeb_store_info *sip = devip2sip(devip, true);
	u8 *fsp = sip->storep;

	if (cmd[0] == PRE_FETCH) {	/* 10 byte cdb */
		lba = get_unaligned_be32(cmd + 2);
		nblks = get_unaligned_be16(cmd + 7);
	} else {			/* PRE-FETCH(16) */
		lba = get_unaligned_be64(cmd + 2);
		nblks = get_unaligned_be32(cmd + 10);
	}
	if (lba + nblks > sdebug_capacity) {
		mk_sense_buffer(scp, ILLEGAL_REQUEST, LBA_OUT_OF_RANGE, 0);
		return check_condition_result;
	}
	if (!fsp)
		goto fini;
	/* PRE-FETCH spec says nothing about LBP or PI so skip them */
	block = do_div(lba, sdebug_store_sectors);
	if (block + nblks > sdebug_store_sectors)
		rest = block + nblks - sdebug_store_sectors;

	/* Try to bring the PRE-FETCH range into CPU's cache */
	sdeb_data_read_lock(sip);
	prefetch_range(fsp + (sdebug_sector_size * block),
		       (nblks - rest) * sdebug_sector_size);
	if (rest)
		prefetch_range(fsp, rest * sdebug_sector_size);

	sdeb_data_read_unlock(sip);
fini:
	if (cmd[1] & 0x2)
		res = SDEG_RES_IMMED_MASK;
	return res | condition_met_result;
}

#define RL_BUCKET_ELEMS 8

/* Even though each pseudo target has a REPORT LUNS "well known logical unit"
 * (W-LUN), the normal Linux scanning logic does not associate it with a
 * device (e.g. /dev/sg7). The following magic will make that association:
 *   "cd /sys/class/scsi_host/host<n> ; echo '- - 49409' > scan"
 * where <n> is a host number. If there are multiple targets in a host then
 * the above will associate a W-LUN to each target. To only get a W-LUN
 * for target 2, then use "echo '- 2 49409' > scan" .
 */
static int resp_report_luns(struct scsi_cmnd *scp,
			    struct sdebug_dev_info *devip)
{
	unsigned char *cmd = scp->cmnd;
	unsigned int alloc_len;
	unsigned char select_report;
	u64 lun;
	struct scsi_lun *lun_p;
	u8 arr[RL_BUCKET_ELEMS * sizeof(struct scsi_lun)];
	unsigned int lun_cnt;	/* normal LUN count (max: 256) */
	unsigned int wlun_cnt;	/* report luns W-LUN count */
	unsigned int tlun_cnt;	/* total LUN count */
	unsigned int rlen;	/* response length (in bytes) */
	int k, j, n, res;
	unsigned int off_rsp = 0;
	const int sz_lun = sizeof(struct scsi_lun);

	clear_luns_changed_on_target(devip);

	select_report = cmd[2];
	alloc_len = get_unaligned_be32(cmd + 6);

	if (alloc_len < 4) {
		pr_err("alloc len too small %d\n", alloc_len);
		mk_sense_invalid_fld(scp, SDEB_IN_CDB, 6, -1);
		return check_condition_result;
	}

	switch (select_report) {
	case 0:		/* all LUNs apart from W-LUNs */
		lun_cnt = sdebug_max_luns;
		wlun_cnt = 0;
		break;
	case 1:		/* only W-LUNs */
		lun_cnt = 0;
		wlun_cnt = 1;
		break;
	case 2:		/* all LUNs */
		lun_cnt = sdebug_max_luns;
		wlun_cnt = 1;
		break;
	case 0x10:	/* only administrative LUs */
	case 0x11:	/* see SPC-5 */
	case 0x12:	/* only subsiduary LUs owned by referenced LU */
	default:
		pr_debug("select report invalid %d\n", select_report);
		mk_sense_invalid_fld(scp, SDEB_IN_CDB, 2, -1);
		return check_condition_result;
	}

	if (sdebug_no_lun_0 && (lun_cnt > 0))
		--lun_cnt;

	tlun_cnt = lun_cnt + wlun_cnt;
	rlen = tlun_cnt * sz_lun;	/* excluding 8 byte header */
	scsi_set_resid(scp, scsi_bufflen(scp));
	pr_debug("select_report %d luns = %d wluns = %d no_lun0 %d\n",
		 select_report, lun_cnt, wlun_cnt, sdebug_no_lun_0);

	/* loops rely on sizeof response header same as sizeof lun (both 8) */
	lun = sdebug_no_lun_0 ? 1 : 0;
	for (k = 0, j = 0, res = 0; true; ++k, j = 0) {
		memset(arr, 0, sizeof(arr));
		lun_p = (struct scsi_lun *)&arr[0];
		if (k == 0) {
			put_unaligned_be32(rlen, &arr[0]);
			++lun_p;
			j = 1;
		}
		for ( ; j < RL_BUCKET_ELEMS; ++j, ++lun_p) {
			if ((k * RL_BUCKET_ELEMS) + j > lun_cnt)
				break;
			int_to_scsilun(lun++, lun_p);
			if (lun > 1 && sdebug_lun_am == SAM_LUN_AM_FLAT)
				lun_p->scsi_lun[0] |= 0x40;
		}
		if (j < RL_BUCKET_ELEMS)
			break;
		n = j * sz_lun;
		res = p_fill_from_dev_buffer(scp, arr, n, off_rsp);
		if (res)
			return res;
		off_rsp += n;
	}
	if (wlun_cnt) {
		int_to_scsilun(SCSI_W_LUN_REPORT_LUNS, lun_p);
		++j;
	}
	if (j > 0)
		res = p_fill_from_dev_buffer(scp, arr, j * sz_lun, off_rsp);
	return res;
}

static int resp_verify(struct scsi_cmnd *scp, struct sdebug_dev_info *devip)
{
	bool is_bytchk3 = false;
	u8 bytchk;
	int ret, j;
	u32 vnum, a_num, off;
	const u32 lb_size = sdebug_sector_size;
	u64 lba;
	u8 *arr;
	u8 *cmd = scp->cmnd;
	struct sdeb_store_info *sip = devip2sip(devip, true);

	bytchk = (cmd[1] >> 1) & 0x3;
	if (bytchk == 0) {
		return 0;	/* always claim internal verify okay */
	} else if (bytchk == 2) {
		mk_sense_invalid_fld(scp, SDEB_IN_CDB, 2, 2);
		return check_condition_result;
	} else if (bytchk == 3) {
		is_bytchk3 = true;	/* 1 block sent, compared repeatedly */
	}
	switch (cmd[0]) {
	case VERIFY_16:
		lba = get_unaligned_be64(cmd + 2);
		vnum = get_unaligned_be32(cmd + 10);
		break;
	case VERIFY:		/* is VERIFY(10) */
		lba = get_unaligned_be32(cmd + 2);
		vnum = get_unaligned_be16(cmd + 7);
		break;
	default:
		mk_sense_invalid_opcode(scp);
		return check_condition_result;
	}
	if (vnum == 0)
		return 0;	/* not an error */
	a_num = is_bytchk3 ? 1 : vnum;
	/* Treat following check like one for read (i.e. no write) access */
	ret = check_device_access_params(scp, lba, a_num, false);
	if (ret)
		return ret;

	arr = kcalloc(lb_size, vnum, GFP_ATOMIC | __GFP_NOWARN);
	if (!arr) {
		mk_sense_buffer(scp, ILLEGAL_REQUEST, INSUFF_RES_ASC,
				INSUFF_RES_ASCQ);
		return check_condition_result;
	}
	/* Not changing store, so only need read access */
	sdeb_data_read_lock(sip);

	ret = do_dout_fetch(scp, a_num, arr);
	if (ret == -1) {
		ret = DID_ERROR << 16;
		goto cleanup;
	} else if (sdebug_verbose && (ret < (a_num * lb_size))) {
		sdev_printk(KERN_INFO, scp->device,
			    "%s: %s: cdb indicated=%u, IO sent=%d bytes\n",
			    my_name, __func__, a_num * lb_size, ret);
	}
	if (is_bytchk3) {
		for (j = 1, off = lb_size; j < vnum; ++j, off += lb_size)
			memcpy(arr + off, arr, lb_size);
	}
	ret = 0;
	if (!comp_write_worker(sip, lba, vnum, arr, true)) {
		mk_sense_buffer(scp, MISCOMPARE, MISCOMPARE_VERIFY_ASC, 0);
		ret = check_condition_result;
		goto cleanup;
	}
cleanup:
	sdeb_data_read_unlock(sip);
	kfree(arr);
	return ret;
}

#define RZONES_DESC_HD 64

/* Report zones depending on start LBA and reporting options */
static int resp_report_zones(struct scsi_cmnd *scp,
			     struct sdebug_dev_info *devip)
{
	unsigned int rep_max_zones, nrz = 0;
	int ret = 0;
	u32 alloc_len, rep_opts, rep_len;
	bool partial;
	u64 lba, zs_lba;
	u8 *arr = NULL, *desc;
	u8 *cmd = scp->cmnd;
	struct sdeb_zone_state *zsp = NULL;
	struct sdeb_store_info *sip = devip2sip(devip, false);

	if (!sdebug_dev_is_zoned(devip)) {
		mk_sense_invalid_opcode(scp);
		return check_condition_result;
	}
	zs_lba = get_unaligned_be64(cmd + 2);
	alloc_len = get_unaligned_be32(cmd + 10);
	if (alloc_len == 0)
		return 0;	/* not an error */
	rep_opts = cmd[14] & 0x3f;
	partial = cmd[14] & 0x80;

	if (zs_lba >= sdebug_capacity) {
		mk_sense_buffer(scp, ILLEGAL_REQUEST, LBA_OUT_OF_RANGE, 0);
		return check_condition_result;
	}

	rep_max_zones = (alloc_len - 64) >> ilog2(RZONES_DESC_HD);

	arr = kzalloc(alloc_len, GFP_ATOMIC | __GFP_NOWARN);
	if (!arr) {
		mk_sense_buffer(scp, ILLEGAL_REQUEST, INSUFF_RES_ASC,
				INSUFF_RES_ASCQ);
		return check_condition_result;
	}

	sdeb_meta_read_lock(sip);

	desc = arr + 64;
	for (lba = zs_lba; lba < sdebug_capacity;
	     lba = zsp->z_start + zsp->z_size) {
		if (WARN_ONCE(zbc_zone(devip, lba) == zsp, "lba = %llu\n", lba))
			break;
		zsp = zbc_zone(devip, lba);
		switch (rep_opts) {
		case 0x00:
			/* All zones */
			break;
		case 0x01:
			/* Empty zones */
			if (zsp->z_cond != ZC1_EMPTY)
				continue;
			break;
		case 0x02:
			/* Implicit open zones */
			if (zsp->z_cond != ZC2_IMPLICIT_OPEN)
				continue;
			break;
		case 0x03:
			/* Explicit open zones */
			if (zsp->z_cond != ZC3_EXPLICIT_OPEN)
				continue;
			break;
		case 0x04:
			/* Closed zones */
			if (zsp->z_cond != ZC4_CLOSED)
				continue;
			break;
		case 0x05:
			/* Full zones */
			if (zsp->z_cond != ZC5_FULL)
				continue;
			break;
		case 0x06:
		case 0x07:
		case 0x10:
			/*
			 * Read-only, offline, reset WP recommended are
			 * not emulated: no zones to report;
			 */
			continue;
		case 0x11:
			/* non-seq-resource set */
			if (!zsp->z_non_seq_resource)
				continue;
			break;
		case 0x3e:
			/* All zones except gap zones. */
			if (zbc_zone_is_gap(zsp))
				continue;
			break;
		case 0x3f:
			/* Not write pointer (conventional) zones */
			if (zbc_zone_is_seq(zsp))
				continue;
			break;
		default:
			mk_sense_buffer(scp, ILLEGAL_REQUEST,
					INVALID_FIELD_IN_CDB, 0);
			ret = check_condition_result;
			goto fini;
		}

		if (nrz < rep_max_zones) {
			/* Fill zone descriptor */
			desc[0] = zsp->z_type;
			desc[1] = zsp->z_cond << 4;
			if (zsp->z_non_seq_resource)
				desc[1] |= 1 << 1;
			put_unaligned_be64((u64)zsp->z_size, desc + 8);
			put_unaligned_be64((u64)zsp->z_start, desc + 16);
			put_unaligned_be64((u64)zsp->z_wp, desc + 24);
			desc += 64;
		}

		if (partial && nrz >= rep_max_zones)
			break;

		nrz++;
	}

	/* Report header */
	/* Zone list length. */
	put_unaligned_be32(nrz * RZONES_DESC_HD, arr + 0);
	/* Maximum LBA */
	put_unaligned_be64(sdebug_capacity - 1, arr + 8);
	/* Zone starting LBA granularity. */
	if (devip->zcap < devip->zsize)
		put_unaligned_be64(devip->zsize, arr + 16);

	rep_len = (unsigned long)desc - (unsigned long)arr;
	ret = fill_from_dev_buffer(scp, arr, min_t(u32, alloc_len, rep_len));

fini:
	sdeb_meta_read_unlock(sip);
	kfree(arr);
	return ret;
}

static int resp_atomic_write(struct scsi_cmnd *scp,
			     struct sdebug_dev_info *devip)
{
	struct sdeb_store_info *sip;
	u8 *cmd = scp->cmnd;
	u16 boundary, len;
	u64 lba, lba_tmp;
	int ret;

	if (!scsi_debug_atomic_write()) {
		mk_sense_invalid_opcode(scp);
		return check_condition_result;
	}

	sip = devip2sip(devip, true);

	lba = get_unaligned_be64(cmd + 2);
	boundary = get_unaligned_be16(cmd + 10);
	len = get_unaligned_be16(cmd + 12);

	lba_tmp = lba;
	if (sdebug_atomic_wr_align &&
	    do_div(lba_tmp, sdebug_atomic_wr_align)) {
		/* Does not meet alignment requirement */
		mk_sense_buffer(scp, ILLEGAL_REQUEST, INVALID_FIELD_IN_CDB, 0);
		return check_condition_result;
	}

	if (sdebug_atomic_wr_gran && len % sdebug_atomic_wr_gran) {
		/* Does not meet alignment requirement */
		mk_sense_buffer(scp, ILLEGAL_REQUEST, INVALID_FIELD_IN_CDB, 0);
		return check_condition_result;
	}

	if (boundary > 0) {
		if (boundary > sdebug_atomic_wr_max_bndry) {
			mk_sense_invalid_fld(scp, SDEB_IN_CDB, 12, -1);
			return check_condition_result;
		}

		if (len > sdebug_atomic_wr_max_length_bndry) {
			mk_sense_invalid_fld(scp, SDEB_IN_CDB, 12, -1);
			return check_condition_result;
		}
	} else {
		if (len > sdebug_atomic_wr_max_length) {
			mk_sense_invalid_fld(scp, SDEB_IN_CDB, 12, -1);
			return check_condition_result;
		}
	}

	ret = do_device_access(sip, scp, 0, lba, len, 0, true, true);
	if (unlikely(ret == -1))
		return DID_ERROR << 16;
	if (unlikely(ret != len * sdebug_sector_size))
		return DID_ERROR << 16;
	return 0;
}

/* Logic transplanted from tcmu-runner, file_zbc.c */
static void zbc_open_all(struct sdebug_dev_info *devip)
{
	struct sdeb_zone_state *zsp = &devip->zstate[0];
	unsigned int i;

	for (i = 0; i < devip->nr_zones; i++, zsp++) {
		if (zsp->z_cond == ZC4_CLOSED)
			zbc_open_zone(devip, &devip->zstate[i], true);
	}
}

static int resp_open_zone(struct scsi_cmnd *scp, struct sdebug_dev_info *devip)
{
	int res = 0;
	u64 z_id;
	enum sdebug_z_cond zc;
	u8 *cmd = scp->cmnd;
	struct sdeb_zone_state *zsp;
	bool all = cmd[14] & 0x01;
	struct sdeb_store_info *sip = devip2sip(devip, false);

	if (!sdebug_dev_is_zoned(devip)) {
		mk_sense_invalid_opcode(scp);
		return check_condition_result;
	}
	sdeb_meta_write_lock(sip);

	if (all) {
		/* Check if all closed zones can be open */
		if (devip->max_open &&
		    devip->nr_exp_open + devip->nr_closed > devip->max_open) {
			mk_sense_buffer(scp, DATA_PROTECT, INSUFF_RES_ASC,
					INSUFF_ZONE_ASCQ);
			res = check_condition_result;
			goto fini;
		}
		/* Open all closed zones */
		zbc_open_all(devip);
		goto fini;
	}

	/* Open the specified zone */
	z_id = get_unaligned_be64(cmd + 2);
	if (z_id >= sdebug_capacity) {
		mk_sense_buffer(scp, ILLEGAL_REQUEST, LBA_OUT_OF_RANGE, 0);
		res = check_condition_result;
		goto fini;
	}

	zsp = zbc_zone(devip, z_id);
	if (z_id != zsp->z_start) {
		mk_sense_buffer(scp, ILLEGAL_REQUEST, INVALID_FIELD_IN_CDB, 0);
		res = check_condition_result;
		goto fini;
	}
	if (zbc_zone_is_conv(zsp)) {
		mk_sense_buffer(scp, ILLEGAL_REQUEST, INVALID_FIELD_IN_CDB, 0);
		res = check_condition_result;
		goto fini;
	}

	zc = zsp->z_cond;
	if (zc == ZC3_EXPLICIT_OPEN || zc == ZC5_FULL)
		goto fini;

	if (devip->max_open && devip->nr_exp_open >= devip->max_open) {
		mk_sense_buffer(scp, DATA_PROTECT, INSUFF_RES_ASC,
				INSUFF_ZONE_ASCQ);
		res = check_condition_result;
		goto fini;
	}

	zbc_open_zone(devip, zsp, true);
fini:
	sdeb_meta_write_unlock(sip);
	return res;
}

static void zbc_close_all(struct sdebug_dev_info *devip)
{
	unsigned int i;

	for (i = 0; i < devip->nr_zones; i++)
		zbc_close_zone(devip, &devip->zstate[i]);
}

static int resp_close_zone(struct scsi_cmnd *scp,
			   struct sdebug_dev_info *devip)
{
	int res = 0;
	u64 z_id;
	u8 *cmd = scp->cmnd;
	struct sdeb_zone_state *zsp;
	bool all = cmd[14] & 0x01;
	struct sdeb_store_info *sip = devip2sip(devip, false);

	if (!sdebug_dev_is_zoned(devip)) {
		mk_sense_invalid_opcode(scp);
		return check_condition_result;
	}

	sdeb_meta_write_lock(sip);

	if (all) {
		zbc_close_all(devip);
		goto fini;
	}

	/* Close specified zone */
	z_id = get_unaligned_be64(cmd + 2);
	if (z_id >= sdebug_capacity) {
		mk_sense_buffer(scp, ILLEGAL_REQUEST, LBA_OUT_OF_RANGE, 0);
		res = check_condition_result;
		goto fini;
	}

	zsp = zbc_zone(devip, z_id);
	if (z_id != zsp->z_start) {
		mk_sense_buffer(scp, ILLEGAL_REQUEST, INVALID_FIELD_IN_CDB, 0);
		res = check_condition_result;
		goto fini;
	}
	if (zbc_zone_is_conv(zsp)) {
		mk_sense_buffer(scp, ILLEGAL_REQUEST, INVALID_FIELD_IN_CDB, 0);
		res = check_condition_result;
		goto fini;
	}

	zbc_close_zone(devip, zsp);
fini:
	sdeb_meta_write_unlock(sip);
	return res;
}

static void zbc_finish_zone(struct sdebug_dev_info *devip,
			    struct sdeb_zone_state *zsp, bool empty)
{
	enum sdebug_z_cond zc = zsp->z_cond;

	if (zc == ZC4_CLOSED || zc == ZC2_IMPLICIT_OPEN ||
	    zc == ZC3_EXPLICIT_OPEN || (empty && zc == ZC1_EMPTY)) {
		if (zc == ZC2_IMPLICIT_OPEN || zc == ZC3_EXPLICIT_OPEN)
			zbc_close_zone(devip, zsp);
		if (zsp->z_cond == ZC4_CLOSED)
			devip->nr_closed--;
		zsp->z_wp = zsp->z_start + zsp->z_size;
		zsp->z_cond = ZC5_FULL;
	}
}

static void zbc_finish_all(struct sdebug_dev_info *devip)
{
	unsigned int i;

	for (i = 0; i < devip->nr_zones; i++)
		zbc_finish_zone(devip, &devip->zstate[i], false);
}

static int resp_finish_zone(struct scsi_cmnd *scp,
			    struct sdebug_dev_info *devip)
{
	struct sdeb_zone_state *zsp;
	int res = 0;
	u64 z_id;
	u8 *cmd = scp->cmnd;
	bool all = cmd[14] & 0x01;
	struct sdeb_store_info *sip = devip2sip(devip, false);

	if (!sdebug_dev_is_zoned(devip)) {
		mk_sense_invalid_opcode(scp);
		return check_condition_result;
	}

	sdeb_meta_write_lock(sip);

	if (all) {
		zbc_finish_all(devip);
		goto fini;
	}

	/* Finish the specified zone */
	z_id = get_unaligned_be64(cmd + 2);
	if (z_id >= sdebug_capacity) {
		mk_sense_buffer(scp, ILLEGAL_REQUEST, LBA_OUT_OF_RANGE, 0);
		res = check_condition_result;
		goto fini;
	}

	zsp = zbc_zone(devip, z_id);
	if (z_id != zsp->z_start) {
		mk_sense_buffer(scp, ILLEGAL_REQUEST, INVALID_FIELD_IN_CDB, 0);
		res = check_condition_result;
		goto fini;
	}
	if (zbc_zone_is_conv(zsp)) {
		mk_sense_buffer(scp, ILLEGAL_REQUEST, INVALID_FIELD_IN_CDB, 0);
		res = check_condition_result;
		goto fini;
	}

	zbc_finish_zone(devip, zsp, true);
fini:
	sdeb_meta_write_unlock(sip);
	return res;
}

static void zbc_rwp_zone(struct sdebug_dev_info *devip,
			 struct sdeb_zone_state *zsp)
{
	enum sdebug_z_cond zc;
	struct sdeb_store_info *sip = devip2sip(devip, false);

	if (!zbc_zone_is_seq(zsp))
		return;

	zc = zsp->z_cond;
	if (zc == ZC2_IMPLICIT_OPEN || zc == ZC3_EXPLICIT_OPEN)
		zbc_close_zone(devip, zsp);

	if (zsp->z_cond == ZC4_CLOSED)
		devip->nr_closed--;

	if (zsp->z_wp > zsp->z_start)
		memset(sip->storep + zsp->z_start * sdebug_sector_size, 0,
		       (zsp->z_wp - zsp->z_start) * sdebug_sector_size);

	zsp->z_non_seq_resource = false;
	zsp->z_wp = zsp->z_start;
	zsp->z_cond = ZC1_EMPTY;
}

static void zbc_rwp_all(struct sdebug_dev_info *devip)
{
	unsigned int i;

	for (i = 0; i < devip->nr_zones; i++)
		zbc_rwp_zone(devip, &devip->zstate[i]);
}

static int resp_rwp_zone(struct scsi_cmnd *scp, struct sdebug_dev_info *devip)
{
	struct sdeb_zone_state *zsp;
	int res = 0;
	u64 z_id;
	u8 *cmd = scp->cmnd;
	bool all = cmd[14] & 0x01;
	struct sdeb_store_info *sip = devip2sip(devip, false);

	if (!sdebug_dev_is_zoned(devip)) {
		mk_sense_invalid_opcode(scp);
		return check_condition_result;
	}

	sdeb_meta_write_lock(sip);

	if (all) {
		zbc_rwp_all(devip);
		goto fini;
	}

	z_id = get_unaligned_be64(cmd + 2);
	if (z_id >= sdebug_capacity) {
		mk_sense_buffer(scp, ILLEGAL_REQUEST, LBA_OUT_OF_RANGE, 0);
		res = check_condition_result;
		goto fini;
	}

	zsp = zbc_zone(devip, z_id);
	if (z_id != zsp->z_start) {
		mk_sense_buffer(scp, ILLEGAL_REQUEST, INVALID_FIELD_IN_CDB, 0);
		res = check_condition_result;
		goto fini;
	}
	if (zbc_zone_is_conv(zsp)) {
		mk_sense_buffer(scp, ILLEGAL_REQUEST, INVALID_FIELD_IN_CDB, 0);
		res = check_condition_result;
		goto fini;
	}

	zbc_rwp_zone(devip, zsp);
fini:
	sdeb_meta_write_unlock(sip);
	return res;
}

static u32 get_tag(struct scsi_cmnd *cmnd)
{
	return blk_mq_unique_tag(scsi_cmd_to_rq(cmnd));
}

/* Queued (deferred) command completions converge here. */
static void sdebug_q_cmd_complete(struct sdebug_defer *sd_dp)
{
	struct sdebug_queued_cmd *sqcp = container_of(sd_dp, struct sdebug_queued_cmd, sd_dp);
	unsigned long flags;
	struct scsi_cmnd *scp = sqcp->scmd;
	struct sdebug_scsi_cmd *sdsc;
	bool aborted;

	if (sdebug_statistics) {
		atomic_inc(&sdebug_completions);
		if (raw_smp_processor_id() != sd_dp->issuing_cpu)
			atomic_inc(&sdebug_miss_cpus);
	}

	if (!scp) {
		pr_err("scmd=NULL\n");
		goto out;
	}

	sdsc = scsi_cmd_priv(scp);
	spin_lock_irqsave(&sdsc->lock, flags);
	aborted = sd_dp->aborted;
	if (unlikely(aborted))
		sd_dp->aborted = false;
	ASSIGN_QUEUED_CMD(scp, NULL);

	spin_unlock_irqrestore(&sdsc->lock, flags);

	if (aborted) {
		pr_info("bypassing scsi_done() due to aborted cmd, kicking-off EH\n");
		blk_abort_request(scsi_cmd_to_rq(scp));
		goto out;
	}

	scsi_done(scp); /* callback to mid level */
out:
	sdebug_free_queued_cmd(sqcp);
}

/* When high resolution timer goes off this function is called. */
static enum hrtimer_restart sdebug_q_cmd_hrt_complete(struct hrtimer *timer)
{
	struct sdebug_defer *sd_dp = container_of(timer, struct sdebug_defer,
						  hrt);
	sdebug_q_cmd_complete(sd_dp);
	return HRTIMER_NORESTART;
}

/* When work queue schedules work, it calls this function. */
static void sdebug_q_cmd_wq_complete(struct work_struct *work)
{
	struct sdebug_defer *sd_dp = container_of(work, struct sdebug_defer,
						  ew.work);
	sdebug_q_cmd_complete(sd_dp);
}

static bool got_shared_uuid;
static uuid_t shared_uuid;

static int sdebug_device_create_zones(struct sdebug_dev_info *devip)
{
	struct sdeb_zone_state *zsp;
	sector_t capacity = get_sdebug_capacity();
	sector_t conv_capacity;
	sector_t zstart = 0;
	unsigned int i;

	/*
	 * Set the zone size: if sdeb_zbc_zone_size_mb is not set, figure out
	 * a zone size allowing for at least 4 zones on the device. Otherwise,
	 * use the specified zone size checking that at least 2 zones can be
	 * created for the device.
	 */
	if (!sdeb_zbc_zone_size_mb) {
		devip->zsize = (DEF_ZBC_ZONE_SIZE_MB * SZ_1M)
			>> ilog2(sdebug_sector_size);
		while (capacity < devip->zsize << 2 && devip->zsize >= 2)
			devip->zsize >>= 1;
		if (devip->zsize < 2) {
			pr_err("Device capacity too small\n");
			return -EINVAL;
		}
	} else {
		if (!is_power_of_2(sdeb_zbc_zone_size_mb)) {
			pr_err("Zone size is not a power of 2\n");
			return -EINVAL;
		}
		devip->zsize = (sdeb_zbc_zone_size_mb * SZ_1M)
			>> ilog2(sdebug_sector_size);
		if (devip->zsize >= capacity) {
			pr_err("Zone size too large for device capacity\n");
			return -EINVAL;
		}
	}

	devip->zsize_shift = ilog2(devip->zsize);
	devip->nr_zones = (capacity + devip->zsize - 1) >> devip->zsize_shift;

	if (sdeb_zbc_zone_cap_mb == 0) {
		devip->zcap = devip->zsize;
	} else {
		devip->zcap = (sdeb_zbc_zone_cap_mb * SZ_1M) >>
			      ilog2(sdebug_sector_size);
		if (devip->zcap > devip->zsize) {
			pr_err("Zone capacity too large\n");
			return -EINVAL;
		}
	}

	conv_capacity = (sector_t)sdeb_zbc_nr_conv << devip->zsize_shift;
	if (conv_capacity >= capacity) {
		pr_err("Number of conventional zones too large\n");
		return -EINVAL;
	}
	devip->nr_conv_zones = sdeb_zbc_nr_conv;
	devip->nr_seq_zones = ALIGN(capacity - conv_capacity, devip->zsize) >>
			      devip->zsize_shift;
	devip->nr_zones = devip->nr_conv_zones + devip->nr_seq_zones;

	/* Add gap zones if zone capacity is smaller than the zone size */
	if (devip->zcap < devip->zsize)
		devip->nr_zones += devip->nr_seq_zones;

	if (devip->zoned) {
		/* zbc_max_open_zones can be 0, meaning "not reported" */
		if (sdeb_zbc_max_open >= devip->nr_zones - 1)
			devip->max_open = (devip->nr_zones - 1) / 2;
		else
			devip->max_open = sdeb_zbc_max_open;
	}

	devip->zstate = kcalloc(devip->nr_zones,
				sizeof(struct sdeb_zone_state), GFP_KERNEL);
	if (!devip->zstate)
		return -ENOMEM;

	for (i = 0; i < devip->nr_zones; i++) {
		zsp = &devip->zstate[i];

		zsp->z_start = zstart;

		if (i < devip->nr_conv_zones) {
			zsp->z_type = ZBC_ZTYPE_CNV;
			zsp->z_cond = ZBC_NOT_WRITE_POINTER;
			zsp->z_wp = (sector_t)-1;
			zsp->z_size =
				min_t(u64, devip->zsize, capacity - zstart);
		} else if ((zstart & (devip->zsize - 1)) == 0) {
			if (devip->zoned)
				zsp->z_type = ZBC_ZTYPE_SWR;
			else
				zsp->z_type = ZBC_ZTYPE_SWP;
			zsp->z_cond = ZC1_EMPTY;
			zsp->z_wp = zsp->z_start;
			zsp->z_size =
				min_t(u64, devip->zcap, capacity - zstart);
		} else {
			zsp->z_type = ZBC_ZTYPE_GAP;
			zsp->z_cond = ZBC_NOT_WRITE_POINTER;
			zsp->z_wp = (sector_t)-1;
			zsp->z_size = min_t(u64, devip->zsize - devip->zcap,
					    capacity - zstart);
		}

		WARN_ON_ONCE((int)zsp->z_size <= 0);
		zstart += zsp->z_size;
	}

	return 0;
}

static struct sdebug_dev_info *sdebug_device_create(
			struct sdebug_host_info *sdbg_host, gfp_t flags)
{
	struct sdebug_dev_info *devip;

	devip = kzalloc(sizeof(*devip), flags);
	if (devip) {
		if (sdebug_uuid_ctl == 1)
			uuid_gen(&devip->lu_name);
		else if (sdebug_uuid_ctl == 2) {
			if (got_shared_uuid)
				devip->lu_name = shared_uuid;
			else {
				uuid_gen(&shared_uuid);
				got_shared_uuid = true;
				devip->lu_name = shared_uuid;
			}
		}
		devip->sdbg_host = sdbg_host;
		if (sdeb_zbc_in_use) {
			devip->zoned = sdeb_zbc_model == BLK_ZONED_HM;
			if (sdebug_device_create_zones(devip)) {
				kfree(devip);
				return NULL;
			}
		} else {
			devip->zoned = false;
		}
		devip->create_ts = ktime_get_boottime();
		atomic_set(&devip->stopped, (sdeb_tur_ms_to_ready > 0 ? 2 : 0));
		spin_lock_init(&devip->list_lock);
		INIT_LIST_HEAD(&devip->inject_err_list);
		list_add_tail(&devip->dev_list, &sdbg_host->dev_info_list);
	}
	return devip;
}

static struct sdebug_dev_info *find_build_dev_info(struct scsi_device *sdev)
{
	struct sdebug_host_info *sdbg_host;
	struct sdebug_dev_info *open_devip = NULL;
	struct sdebug_dev_info *devip;

	sdbg_host = shost_to_sdebug_host(sdev->host);

	list_for_each_entry(devip, &sdbg_host->dev_info_list, dev_list) {
		if ((devip->used) && (devip->channel == sdev->channel) &&
		    (devip->target == sdev->id) &&
		    (devip->lun == sdev->lun))
			return devip;
		else {
			if ((!devip->used) && (!open_devip))
				open_devip = devip;
		}
	}
	if (!open_devip) { /* try and make a new one */
		open_devip = sdebug_device_create(sdbg_host, GFP_ATOMIC);
		if (!open_devip) {
			pr_err("out of memory at line %d\n", __LINE__);
			return NULL;
		}
	}

	open_devip->channel = sdev->channel;
	open_devip->target = sdev->id;
	open_devip->lun = sdev->lun;
	open_devip->sdbg_host = sdbg_host;
	set_bit(SDEBUG_UA_POOCCUR, open_devip->uas_bm);
	open_devip->used = true;
	return open_devip;
}

static int scsi_debug_slave_alloc(struct scsi_device *sdp)
{
	if (sdebug_verbose)
		pr_info("slave_alloc <%u %u %u %llu>\n",
		       sdp->host->host_no, sdp->channel, sdp->id, sdp->lun);

	return 0;
}

static int scsi_debug_slave_configure(struct scsi_device *sdp)
{
	struct sdebug_dev_info *devip =
			(struct sdebug_dev_info *)sdp->hostdata;
	struct dentry *dentry;

	if (sdebug_verbose)
		pr_info("slave_configure <%u %u %u %llu>\n",
		       sdp->host->host_no, sdp->channel, sdp->id, sdp->lun);
	if (sdp->host->max_cmd_len != SDEBUG_MAX_CMD_LEN)
		sdp->host->max_cmd_len = SDEBUG_MAX_CMD_LEN;
	if (devip == NULL) {
		devip = find_build_dev_info(sdp);
		if (devip == NULL)
			return 1;  /* no resources, will be marked offline */
	}
	sdp->hostdata = devip;
	if (sdebug_no_uld)
		sdp->no_uld_attach = 1;
	config_cdb_len(sdp);

	if (sdebug_allow_restart)
		sdp->allow_restart = 1;

	devip->debugfs_entry = debugfs_create_dir(dev_name(&sdp->sdev_dev),
				sdebug_debugfs_root);
	if (IS_ERR_OR_NULL(devip->debugfs_entry))
		pr_info("%s: failed to create debugfs directory for device %s\n",
			__func__, dev_name(&sdp->sdev_gendev));

	dentry = debugfs_create_file("error", 0600, devip->debugfs_entry, sdp,
				&sdebug_error_fops);
	if (IS_ERR_OR_NULL(dentry))
		pr_info("%s: failed to create error file for device %s\n",
			__func__, dev_name(&sdp->sdev_gendev));

	return 0;
}

static void scsi_debug_slave_destroy(struct scsi_device *sdp)
{
	struct sdebug_dev_info *devip =
		(struct sdebug_dev_info *)sdp->hostdata;
	struct sdebug_err_inject *err;

	if (sdebug_verbose)
		pr_info("slave_destroy <%u %u %u %llu>\n",
		       sdp->host->host_no, sdp->channel, sdp->id, sdp->lun);

	if (!devip)
		return;

	spin_lock(&devip->list_lock);
	list_for_each_entry_rcu(err, &devip->inject_err_list, list) {
		list_del_rcu(&err->list);
		call_rcu(&err->rcu, sdebug_err_free);
	}
	return devip;
}

static s                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   