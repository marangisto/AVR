#ifndef PINS_H
#define PINS_H

#include <avr/io.h>
#include "Traits.h"

template<class PORT> struct port_t
{
	static inline volatile uint8_t& ddr();
	static inline volatile uint8_t& port();
	static inline const volatile uint8_t& pin();
};

template<class PORT, unsigned PIN> struct pin_t : port_t<PORT>
{
	static_assert(PIN < 8, "pin bit out of range");

	static const int pin = PIN;
	static const uint8_t mask = 1 << PIN;
};

struct PB;

#if defined(PC)
struct PC;
#endif

#if defined(PD)
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

#if defined(PC)
template<> struct port_t<PC>
{
	typedef PC port;
	static inline volatile uint8_t& ddr() { return DDRC; }
	static inline volatile uint8_t& reg() { return PORTC; }
	static inline volatile const uint8_t& pin() { return PINC; }
};
#endif

#if defined(PD)
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

template<class T0, class T1, class T2, class T3, class T4, class T5, class T6, class T7>
static inline void check_union_mask()
{
	static_assert(std::is_same<typename T0::port, typename T1::port>(), "2nd pin on different port");
	static_assert(std::is_same<typename T0::port, typename T2::port>(), "3rd pin on different port");
	static_assert(std::is_same<typename T0::port, typename T3::port>(), "4th pin on different port");
	static_assert(std::is_same<typename T0::port, typename T4::port>(), "5th pin on different port");
	static_assert(std::is_same<typename T0::port, typename T5::port>(), "6th pin on different port");
	static_assert(std::is_same<typename T0::port, typename T6::port>(), "7th pin on different port");
	static_assert(std::is_same<typename T0::port, typename T7::port>(), "8th pin on different port");
}

template<class T0, class T1, class T2, class T3, class T4, class T5, class T6, class T7>
static inline uint8_t union_mask()
{
	check_union_mask<T0, T1, T2, T3, T4, T5, T6, T7>();
	return T0::mask | T1::mask | T2::mask | T3::mask | T4::mask | T5::mask | T6::mask | T7::mask;
}

template<class T0, class T1, class T2, class T3, class T4, class T5, class T6, class T7>
static inline uint8_t invert_union_mask()
{
	check_union_mask<T0, T1, T2, T3, T4, T5, T6, T7>();
	return 0xff ^ (T0::mask | T1::mask | T2::mask | T3::mask | T4::mask | T5::mask | T6::mask | T7::mask);
}

template<class T0, class T1 = T0, class T2 = T0, class T3 = T0, class T4 = T0, class T5 = T0, class T6 = T0, class T7 = T0>
static inline void digital_in()
{
	T0::ddr() &= invert_union_mask<T0, T1, T2, T3, T4, T5, T6, T7>();
}

template<class T0, class T1 = T0, class T2 = T0, class T3 = T0, class T4 = T0, class T5 = T0, class T6 = T0, class T7 = T0>
static inline void digital_out()
{
	T0::ddr() |= union_mask<T0, T1, T2, T3, T4, T5, T6, T7>();
}

template<class T0, class T1 = T0, class T2 = T0, class T3 = T0, class T4 = T0, class T5 = T0, class T6 = T0, class T7 = T0>
static inline void set()
{
	T0::reg() |= union_mask<T0, T1, T2, T3, T4, T5, T6, T7>();
}

template<class T0, class T1 = T0, class T2 = T0, class T3 = T0, class T4 = T0, class T5 = T0, class T6 = T0, class T7 = T0>
static inline void clear()
{
	T0::reg() &= invert_union_mask<T0, T1, T2, T3, T4, T5, T6, T7>();
}

template<class T0, class T1 = T0, class T2 = T0, class T3 = T0, class T4 = T0, class T5 = T0, class T6 = T0, class T7 = T0>
static inline void toggle()
{
	T0::reg() ^= union_mask<T0, T1, T2, T3, T4, T5, T6, T7>();
}

template<class T0, class T1 = T0, class T2 = T0, class T3 = T0, class T4 = T0, class T5 = T0, class T6 = T0, class T7 = T0>
static inline void write(bool x)
{
	x ? set<T0, T1, T2, T3, T4, T5, T6, T7>() : clear<T0, T1, T2, T3, T4, T5, T6, T7>();
}

template<class T0>
static inline bool read()
{
	return (T0::pin() & T0::mask) != 0;
}

template<class T0, class T1>
static inline void read(bool& b0, bool& b1)
{
	static_assert(std::is_same<typename T0::port, typename T1::port>(), "2nd pin on different port");

	uint8_t x = T0::pin();

	b0 = (x & T0::mask) != 0;
	b1 = (x & T1::mask) != 0;
}

template<class T0, class T1, class T2>
static inline void read(bool& b0, bool& b1, bool& b2)
{
	static_assert(std::is_same<typename T0::port, typename T1::port>(), "2nd pin on different port");
	static_assert(std::is_same<typename T0::port, typename T2::port>(), "3rd pin on different port");

	uint8_t x = T0::pin();

	b0 = (x & T0::mask) != 0;
	b1 = (x & T1::mask) != 0;
	b2 = (x & T2::mask) != 0;
}

template<class T0, class T1, class T2, class T3>
static inline void read(bool& b0, bool& b1, bool& b2, bool& b3)
{
	static_assert(std::is_same<typename T0::port, typename T1::port>(), "2nd pin on different port");
	static_assert(std::is_same<typename T0::port, typename T2::port>(), "3rd pin on different port");
	static_assert(std::is_same<typename T0::port, typename T3::port>(), "4th pin on different port");

	uint8_t x = T0::pin();

	b0 = (x & T0::mask) != 0;
	b1 = (x & T1::mask) != 0;
	b2 = (x & T2::mask) != 0;
	b3 = (x & T3::mask) != 0;
}

#endif // PINS_H

