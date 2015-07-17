#ifndef PINS_H
#define PINS_H

// missing <type_traits> on avr-g++?

namespace std
{
	template<class T, T v>
	struct integral_constant {
    	static constexpr T value = v;
    	typedef T value_type;
    	typedef integral_constant type;
    	constexpr operator value_type() const noexcept { return value; }
    	constexpr value_type operator()() const noexcept { return value; } //since c++14
	};

	typedef integral_constant<bool, true> true_type;
	typedef integral_constant<bool, false> false_type;

	template<class T, class U>
	struct is_same : false_type {};

	template<class T>
	struct is_same<T, T> : true_type {};
}

template<class PORT> struct port_t
{
	static inline volatile uint8_t& ddr();
	static inline volatile uint8_t& port();
	static inline const volatile uint8_t& pin();
};

template<class PORT, unsigned PIN> struct pin_t : port_t<PORT>
{
	static_assert(PIN < 8, "pin bit out of range");
	static inline uint8_t mask() { return 1 << PIN; }
};

struct PB;
struct PC;
struct PD;

template<> struct port_t<PB>
{
	typedef PB ty;
	static inline volatile uint8_t& ddr() { return DDRB; }
	static inline volatile uint8_t& port() { return PORTB; }
	static inline volatile const uint8_t& pin() { return PINB; }
};

template<> struct port_t<PC>
{
	typedef PC ty;
	static inline volatile uint8_t& ddr() { return DDRC; }
	static inline volatile uint8_t& port() { return PORTC; }
	static inline volatile const uint8_t& pin() { return PINC; }
};

template<> struct port_t<PD>
{
	typedef PD ty;
	static inline volatile uint8_t& ddr() { return DDRD; }
	static inline volatile uint8_t& port() { return PORTD; }
	static inline volatile const uint8_t& pin() { return PIND; }
};

template<class T0, class T1, class T2, class T3, class T4, class T5, class T6, class T7>
static inline void check_union_mask()
{
	static_assert(std::is_same<typename T0::ty, typename T1::ty>(), "2nd pin on different port");
	static_assert(std::is_same<typename T0::ty, typename T2::ty>(), "3rd pin on different port");
	static_assert(std::is_same<typename T0::ty, typename T3::ty>(), "4th pin on different port");
	static_assert(std::is_same<typename T0::ty, typename T4::ty>(), "5th pin on different port");
	static_assert(std::is_same<typename T0::ty, typename T5::ty>(), "6th pin on different port");
	static_assert(std::is_same<typename T0::ty, typename T6::ty>(), "7th pin on different port");
	static_assert(std::is_same<typename T0::ty, typename T7::ty>(), "8th pin on different port");
}

template<class T0, class T1, class T2, class T3, class T4, class T5, class T6, class T7>
static inline uint8_t union_mask()
{
	check_union_mask<T0, T1, T2, T3, T4, T5, T6, T7>();
	return T0::mask() | T1::mask() | T2::mask() | T3::mask() | T4::mask() | T5::mask() | T6::mask() | T7::mask();
}

template<class T0, class T1, class T2, class T3, class T4, class T5, class T6, class T7>
static inline uint8_t invert_union_mask()
{
	check_union_mask<T0, T1, T2, T3, T4, T5, T6, T7>();
	return 0xff ^ (T0::mask() | T1::mask() | T2::mask() | T3::mask() | T4::mask() | T5::mask() | T6::mask() | T7::mask());
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
	T0::port() |= union_mask<T0, T1, T2, T3, T4, T5, T6, T7>();
}

template<class T0, class T1 = T0, class T2 = T0, class T3 = T0, class T4 = T0, class T5 = T0, class T6 = T0, class T7 = T0>
static inline void clear()
{
	T0::port() &= invert_union_mask<T0, T1, T2, T3, T4, T5, T6, T7>();
}

template<class T0, class T1 = T0, class T2 = T0, class T3 = T0, class T4 = T0, class T5 = T0, class T6 = T0, class T7 = T0>
static inline void toggle()
{
	T0::port() ^= union_mask<T0, T1, T2, T3, T4, T5, T6, T7>();
}

template<class T0, class T1 = T0, class T2 = T0, class T3 = T0, class T4 = T0, class T5 = T0, class T6 = T0, class T7 = T0>
static inline void write(bool x)
{
	x ? set<T0, T1, T2, T3, T4, T5, T6, T7>() : clear<T0, T1, T2, T3, T4, T5, T6, T7>();
}

template<class T0>
static inline bool read()
{
	return (T0::pin() & T0::mask()) != 0;
}

template<class T0, class T1>
static inline void read(bool& b0, bool& b1)
{
	static_assert(std::is_same<typename T0::ty, typename T1::ty>(), "2nd pin on different port");

	uint8_t x = T0::pin();

	b0 = (x & T0::mask()) != 0;
	b1 = (x & T1::mask()) != 0;
}

template<class T0, class T1, class T2>
static inline void read(bool& b0, bool& b1, bool& b2)
{
	static_assert(std::is_same<typename T0::ty, typename T1::ty>(), "2nd pin on different port");
	static_assert(std::is_same<typename T0::ty, typename T2::ty>(), "3rd pin on different port");

	uint8_t x = T0::pin();

	b0 = (x & T0::mask()) != 0;
	b1 = (x & T1::mask()) != 0;
	b2 = (x & T2::mask()) != 0;
}

template<class T0, class T1, class T2, class T3>
static inline void read(bool& b0, bool& b1, bool& b2, bool& b3)
{
	static_assert(std::is_same<typename T0::ty, typename T1::ty>(), "2nd pin on different port");
	static_assert(std::is_same<typename T0::ty, typename T2::ty>(), "3rd pin on different port");
	static_assert(std::is_same<typename T0::ty, typename T3::ty>(), "4th pin on different port");

	uint8_t x = T0::pin();

	b0 = (x & T0::mask()) != 0;
	b1 = (x & T1::mask()) != 0;
	b2 = (x & T2::mask()) != 0;
	b3 = (x & T3::mask()) != 0;
}

#endif // PINS_H

