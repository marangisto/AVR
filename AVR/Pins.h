#pragma once

#include <avr/io.h>
#include "Traits.h"

template<class PORT> struct port_t
{
	static inline volatile uint8_t& ddr();
	static inline volatile uint8_t& port();
	static inline const volatile uint8_t& pin();
};

static const bool enable_pullup = true;

template<class PORT, unsigned BIT, bool PULLUP = false> struct input_t : port_t<PORT>
{
	static_assert(BIT < 8, "bit out of range");

	static const int bit = BIT;
	static const uint8_t mask = 1 << BIT;

	static void setup()
	{
		port_t<PORT>::ddr() &= ~mask;
		if (PULLUP)
			port_t<PORT>::reg() |= mask;
	}

	static bool read()
	{
		return (port_t<PORT>::pin() & mask) != 0;
	}
};

template<class PORT, unsigned BIT> struct output_t : port_t<PORT>
{
	static_assert(BIT < 8, "bit out of range");

	static const int bit = BIT;
	static const uint8_t mask = 1 << BIT;

	static inline void setup()
	{
		port_t<PORT>::ddr() |= mask;
	}

	static inline void set()
	{
		port_t<PORT>::reg() |= mask;
	}

	static inline void clear()
	{
		port_t<PORT>::reg() &= ~mask;
	}

	static inline void write(bool x)
	{
		x ? set() : clear();
	}

	static inline void toggle()
	{
		port_t<PORT>::reg() ^= mask;
	}
};

struct NO_PORT;

template<> struct port_t<NO_PORT>
{
	typedef NO_PORT port;
};

typedef output_t<NO_PORT, 0> no_output_t;

struct PB;

#if defined(__AVR_ATmega328P__) || defined(__AVR_ATmega328__) || defined(__AVR_ATmega32U4__)
struct PC;
#endif

#if defined(__AVR_ATmega328P__) || defined(__AVR_ATmega328__) || defined(__AVR_ATmega32U4__)
struct PD;
#endif

#if defined(PE)
struct PE;
#endif

template<> struct port_t<PB>
{
	typedef PB port;
	static inline volatile uint8_t& ddr() { return DDRB; }
	static inline volatile uint8_t& reg() { return PORTB; }
	static inline volatile const uint8_t& pin() { return PINB; }
};

#if defined(__AVR_ATmega328P__) || defined(__AVR_ATmega328__) || defined(__AVR_ATmega32U4__)
template<> struct port_t<PC>
{
	typedef PC port;
	static inline volatile uint8_t& ddr() { return DDRC; }
	static inline volatile uint8_t& reg() { return PORTC; }
	static inline volatile const uint8_t& pin() { return PINC; }
};
#endif

#if defined(__AVR_ATmega328P__) || defined(__AVR_ATmega328__) || defined(__AVR_ATmega32U4__)
template<> struct port_t<PD>
{
	typedef PD port;
	static inline volatile uint8_t& ddr() { return DDRD; }
	static inline volatile uint8_t& reg() { return PORTD; }
	static inline volatile const uint8_t& pin() { return PIND; }
};
#endif

#if defined(PE)
template<> struct port_t<PE>
{
	typedef PE port;
	static inline volatile uint8_t& ddr() { return DDRE; }
	static inline volatile uint8_t& reg() { return PORTE; }
	static inline volatile const uint8_t& pin() { return PINE; }
};
#endif

