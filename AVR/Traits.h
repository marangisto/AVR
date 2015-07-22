#ifndef TRAITS_H
#define TRAITS_H

// coz of missing <type_traits> on avr-g++?

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
	static const int pin = PIN;

	static_assert(PIN < 8, "pin bit out of range");
	static inline uint8_t mask() { return 1 << PIN; }
};

#endif // TRAITS_H

