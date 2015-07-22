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

#endif // TRAITS_H

