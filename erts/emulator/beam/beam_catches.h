/*
 * beam_catches.h
 *
 * Copyright (c) 2000  Mikael Pettersson
 */
#ifndef __BEAM_CATCHES_H
#define __BEAM_CATCHES_H

#define BEAM_CATCHES_NIL	(-1)

extern void beam_catches_init(void);
extern unsigned beam_catches_cons(uint32 *cp, unsigned cdr);
extern uint32 *beam_catches_car(unsigned i);
extern void beam_catches_delmod(unsigned head, uint32 *code, unsigned code_bytes);

#define catch_pc(x)	beam_catches_car(catch_val((x)))

#endif	/* __BEAM_CATCHES_H */
