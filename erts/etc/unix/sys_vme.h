/*
 * This file is taken from the FORCEvme package, containing
 * VME drivers for the FORCE CPU-3CE/5CE/5TE and Solaris 2.[45]
 */

/* based on vme.h 1.4 for sun4e, modified for Force CPU-3CE */

#ifndef	_SYS_VME_H
#define	_SYS_VME_H

#if !defined(__GNUC__)
#pragma ident   "@(#)vme.h 1.1   93/07/13" /* */
#endif

#ifdef  __cplusplus
extern "C" {
#endif

/*
 * VME Interface Registers
 */

#define	OBIO_VME_ADDR	0x3FE00000	/* physical address of VME registers */
#define VME_CTL_ADDR	0x71380000

/*
 * physical address of the VMEbus window
 */
#define SBUS_VME_WIN	0x60000000	/* 256 MB are mapped into sbus slot 3 */

/*
 * VME interface register offsets
 */
#define	LOCKER_OFFSET		0x00

#define	IACK1_OFFSET		0x03
#define	IACK2_OFFSET		0x05
#define	IACK3_OFFSET		0x07
#define	IACK4_OFFSET		0x09
#define	IACK5_OFFSET		0x0B
#define	IACK6_OFFSET		0x0D
#define	IACK7_OFFSET		0x0F

#define	MBOX_OFFSET		0x10	/* mailbox register */
#define	INTENABLE_OFFSET	0x14	/* interrupt enable register */
#define	A32MAP_OFFSET		0x18
#define	SLAVEMAP_OFFSET		0x1C


/*
 * Flags/masks for VME registers
 */

/*
 * Bus Locker flags/masks.
 */

#define	VME_LOCKENABLE	0x01	/* Enable Bus Locker Capability */
#define	VME_BUSREQ	0x02	/* Request VME Bus */
#define	VME_OWNED	0x80	/* Flag: 1 = VME Bus is owned */

/*
 * Mailbox register flags/masks.
 */

#define	VME_MBOXINTPEND	0x80	/* Mailbox interrupt is pending */
#define	VME_MBOXENABLE	0x40	/* Mailbox interrupt enable */

/*
 * Interrupt Handler flags/masks.
 */

#define	VME_ROUNDROBIN	0x01	/*
				 * Flag: 1 = Round Robin Arbiter;
				 * 0 = Single Level
				 */
#define	VME_ENABIRQ1	0x02	/* Enable VME Interrupt 1 */
#define	VME_ENABIRQ2	0x04	/* Enable VME Interrupt 2 */
#define	VME_ENABIRQ3	0x08	/* Enable VME Interrupt 3 */
#define	VME_ENABIRQ4	0x10	/* Enable VME Interrupt 4 */
#define	VME_ENABIRQ5	0x20	/* Enable VME Interrupt 5 */
#define	VME_ENABIRQ6	0x40	/* Enable VME Interrupt 6 */
#define	VME_ENABIRQ7	0x80	/* Enable VME Interrupt 7 */

#define	VME_IRQENABLE_BITS	"\20\10IRQ7\7IRQ6\6IRQ5\5IRQ4\4IRQ3\3IRQ2\2IRQ1"

/*
 * A32 Map register flags/masks.
 */

#define	VME_LOOPB	0x01	/* VME loopback mode */

/*
 * Slavemap register flags/masks.
 */

#define	VME_BLOCKMODE	0x80	/* mask to enable/disable block mode xfers */
#define	VME_SDVMAMASK	0x0F	/* mask for SDVMA window base address */

#define	VEC_MIN 0
#define	VEC_MAX 255

#ifndef	_ASM

typedef struct vmevec {
	u_int	(*func)();
	caddr_t	arg;
	void	*mutex;	/* declared as void * here */
} vmevec;


typedef struct vme_interface
{
	unsigned char	vme_locker;		/* r/w */
	unsigned char	pad1;
	unsigned char	pad2;
	unsigned char	vme_iack1;		/* r/o */
	unsigned char	vme_intmonitor;		/* S4 Rev. > 1 */
	unsigned char	vme_iack2;		/* r/o */
	unsigned char	pad4;
	unsigned char	vme_iack3;		/* r/o */
	unsigned char	vme_rerun;		/* r/w, mbox int lvl & rerun */
	unsigned char	vme_iack4;		/* r/o */
	unsigned char	pad6;
	unsigned char	vme_iack5;		/* r/o */
	unsigned char	pad7;
	unsigned char	vme_iack6;		/* r/o */
	unsigned char	pad8;
	unsigned char	vme_iack7;		/* r/o */
	unsigned char	vme_mbox;		/* r/w */
	unsigned char	pad9;
	unsigned char	pad10;
	unsigned char	pad11;
	unsigned char	vme_intenable;		/* r/w */
	unsigned char	pad12;
	unsigned char	pad13;
	unsigned char	pad14;
	unsigned char	vme_a32map;		/* r/w */
	unsigned char	pad15;
	unsigned char	pad16;
	unsigned char	pad17;
	unsigned char	vme_slavemap;		/* r/w */
} VME_INTERFACE;

typedef struct vme_ctl_reg {
    unsigned char vme_slavebase1; /* w: serial loading, r: A27-A24 upper */
    unsigned char vme_slavebase2; /* w: clear ABORT irq, r: A27-A24 lower */
    unsigned char vme_slavebase3; /* w:clear SYSFAIL irq,r:slave addr A31-A28 */
    unsigned char vme_ctl;	/* r/w */
    unsigned char vme_a32map;	/* r/w */
    unsigned char vme_gpr1;	/* r, w:bit 6 sysfail to VME */
    unsigned char led_display;	/* w */
    unsigned char vme_gpr2;	/* r/w */
} VME_CTL;



struct vme_ioctl {
	int reg_no;
	int reg_data; /* typecast this in your driver (usually to u_char) !! */
};

struct vme_ioctl_map {
	unsigned long data_size;
        unsigned long virt_addr;
	unsigned long dvma_addr;
};

struct vme_ioctl_dvma {
	unsigned long base;
	unsigned long limit;
};

#endif	/* _ASM */

#ifdef	__cplusplus
}
#endif

#define BT_VME_D16_USER	0x0A	/* bustype: bit 35..32 of addr */
#define BT_VME_D32_USER 0x0B
#define BT_VME_D16_SUPV	0x0C	/* bustype: bit 35..32 of addr */
#define BT_VME_D32_SUPV 0x0D

/* values for the vme_ctl register */
#define VME_CTL_A24SLAVE 0x10	/* slave address mode A24 */
#define VME_CTL_SUPV	0x20	/* enables Supervisory VMEbus accesses */
#define VME_CTL_OLD_DVMA 0x40	/* select the old S4/sun4 dvma mode */

/* values for the vme_a32map register */
#define VME_WATCHDOG_ENA 0x20	/* enables watchdog, can not be disabled !! */

/* values for the vme_gpr1 register */
#define VME_ABORT_PEND	0x10	/* ABORT IRQ pending */
#define VME_SYSFAIL_PEND 0x20	/* SYSFAIL IRQ is pending */
#define VME_SYSFAIL_H	0x40	/* set the VME SYSFAIL line to HIGH (inactiv) */
#define VME_SYSFAIL_STAT 0x80	/* VME SYSFAIL line status (1 = inactiv) */

/* values for the vme_gpr2 register */
#define VME_SELBOOTEPR	0x01	/* selects Boot Flash Eprom (0=System) */
#define VME_SELEPRDEV2	0x02	/* selects Sys Flash Eprom#2 (0= #1) */
#define VME_PFDIRQ_ENA	0x04	/* enable pfd pin IRQs */
					/* (ACFAIL, ABORT, WATCHDOG, SYSFAIL) */
#define IRQ15_ENA	0x04	/* just another name */
#define VME_DVMA_ENA	0x08	/* enable DVMA accesses */
#define VME_ACFAIL_PEND	0x10	/* ACFAIL IRQ is pending */
#define VME_ACFAIL_RES	0x20	/* ACFAIL IRQ reset (write a '1') */
#define VME_ACFAIL_STAT	0x40	/* VME ACFAIL status (low = activ) */
#define VME_WATCHDOG_RES	0x80	/* WATCHDOG IRQ reset (write a '1') */
 
/* minor number assignments */
#define VME16D16        0
#define VME24D16        1
#define VME32D16        2
#define VME16D32        3
#define VME24D32        4
#define VME32D32        5
#define VMEDVMA         6

#define VME_MBOX_INT	0x80


#define VME16_BASE      0xFFFF0000
#define VME16_SIZE      (1<<16)
#define VME16_MASK      (VME16_SIZE-1)
 
#define VME24_BASE      0xFF000000
#define VME24_SIZE      (1<<24)
#define VME24_MASK      (VME24_SIZE-1)

#define VME32_BASE	0
#define VME32_SIZE	0xFF000000
 
/* definitions for ioctls */
#define VME_SET_REG	1
#define VME_GET_REG	2
#define VME_MAP_SLAVE   3
#define VME_UNMAP_SLAVE 4
#define VMEMBOX_WAIT	10
#define SET_ABORT_PID	11  /* set process id to sent signal upon ABORT switch */
#define VME_SET_SLAVE_WIN	12
#define VME_GET_SLAVE_WIN	13
#define VME_SET_VME_WIN	14
#define VME_GET_VME_WIN 15

/*
 * register numbers for VME_SET_REG/VME_GET_REG ioctl's
 */
#define SLAVEMAP	7
#define VME_A32MAP_REG	10	/* this register is NOT the S4 A32MAP register */
#define VME_CTL_REG	11
#define VME_GPR_REG	12

#endif	/* !_SYS_VME_H */
