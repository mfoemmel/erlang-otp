/*
 * beam_catches.h
 *
 * Copyright (c) 2000  Mikael Pettersson
 */
#ifndef __BEAM_CATCHES_H
#define __BEAM_CATCHES_H

#define BEAM_CATCHES_NIL	(-1)

void beam_catches_init(void);
unsigned beam_catches_cons(Eterm* cp, unsigned cdr);
Eterm *beam_catches_car(unsigned i);
void beam_catches_delmod(unsigned head, Eterm* code, unsigned code_bytes);

#define catch_pc(x)	beam_catches_car(catch_val((x)))

#endif	/* __BEAM_CATCHES_H */
