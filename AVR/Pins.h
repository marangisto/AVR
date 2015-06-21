#ifndef PINS_H
#define PINS_H

template<class PORT> struct port_t
{
	static inline volatile uint8_t& ddr();
	static inline volatile uint8_t& port();
	static inline const volatile uint8_t& pin();
};

template<class PORT, int PIN> struct pin_t : port_t<PORT>
{
	static inline uint8_t mask() { return 1 << PIN; }
};

struct PB;;
struct PC;;
struct PD;;

template<> struct port_t<PB>
{
	static inline volatile uint8_t& ddr() { return DDRB; }
	static inline volatile uint8_t& port() { return PORTB; }
	static inline volatile const uint8_t& pin() { return PINB; }
};

template<> struct port_t<PC>
{
	static inline volatile uint8_t& ddr() { return DDRC; }
	static inline volatile uint8_t& port() { return PORTC; }
	static inline volatile const uint8_t& pin() { return PINC; }
};

template<> struct port_t<PD>
{
	static inline volatile uint8_t& ddr() { return DDRD; }
	static inline volatile uint8_t& port() { return PORTD; }
	static inline volatile const uint8_t& pin() { return PIND; }
};


template<class T> static inline void digital_in()
{
	T::ddr() &= ~T::mask();
}

template<class T> static inline void digital_out()
{
	T::ddr() |= T::mask();
}

template<class T> static inline void set()
{
	T::port() |= T::mask();
}

template<class T> static inline void clear()
{
	T::port() &= ~T::mask();
}

template<class T> static inline void toggle()
{
	T::port() ^= T::mask();
}

template<class T> static inline void write(bool x)
{
	x ? set<T>() : clear<T>();
}

#endif // PINS_H

