#ifndef PINS_H
#define PINS_H

template<class PIN> struct digital_pin
{
	static inline volatile uint8_t& ddr();
	static inline volatile uint8_t& port();
	static inline const volatile uint8_t& pin();
	static inline uint8_t mask();
};

struct Pb0;
struct Pb1;
struct Pb2;
struct Pb3;
struct Pb4;
struct Pb5;
struct Pb6;
struct Pb7;

struct Pc0;
struct Pc1;
struct Pc2;
struct Pc3;
struct Pc4;
struct Pc5;
struct Pc6;
struct Pc7;

struct Pd0;
struct Pd1;
struct Pd2;
struct Pd3;
struct Pd4;
struct Pd5;
struct Pd6;
struct Pd7;

template<> struct digital_pin<Pb0>
{
	static inline volatile uint8_t& ddr() { return DDRB; }
	static inline volatile uint8_t& port() { return PORTB; }
	static inline volatile const uint8_t& pin() { return PINB; }
	static inline uint8_t mask() { return 1 << 0; }
};

template<> struct digital_pin<Pb1>
{
	static inline volatile uint8_t& ddr() { return DDRB; }
	static inline volatile uint8_t& port() { return PORTB; }
	static inline const volatile uint8_t& pin() { return PINB; }
	static inline uint8_t mask() { return 1 << 1; }
};

template<> struct digital_pin<Pb2>
{
	static inline volatile uint8_t& ddr() { return DDRB; }
	static inline volatile uint8_t& port() { return PORTB; }
	static inline const volatile uint8_t& pin() { return PINB; }
	static inline uint8_t mask() { return 1 << 2; }
};

template<> struct digital_pin<Pb3>
{
	static inline volatile uint8_t& ddr() { return DDRB; }
	static inline volatile uint8_t& port() { return PORTB; }
	static inline const volatile uint8_t& pin() { return PINB; }
	static inline uint8_t mask() { return 1 << 3; }
};

template<> struct digital_pin<Pb4>
{
	static inline volatile uint8_t& ddr() { return DDRB; }
	static inline volatile uint8_t& port() { return PORTB; }
	static inline const volatile uint8_t& pin() { return PINB; }
	static inline uint8_t mask() { return 1 << 4; }
};

template<> struct digital_pin<Pb5>
{
	static inline volatile uint8_t& ddr() { return DDRB; }
	static inline volatile uint8_t& port() { return PORTB; }
	static inline const volatile uint8_t& pin() { return PINB; }
	static inline uint8_t mask() { return 1 << 5; }
};

/* NOT AVAILABLE ON Atmega-328 (used for crystal)
template<> struct digital_pin<Pb6>
{
	static inline volatile uint8_t& ddr() { return DDRB; }
	static inline volatile uint8_t& port() { return PORTB; }
	static inline const volatile uint8_t& pin() { return PINB; }
	static inline uint8_t mask() { return 1 << 6; }
};

template<> struct digital_pin<Pb7>
{
	static inline volatile uint8_t& ddr() { return DDRB; }
	static inline volatile uint8_t& port() { return PORTB; }
	static inline const volatile uint8_t& pin() { return PINB; }
	static inline uint8_t mask() { return 1 << 7; }
};
*/

template<> struct digital_pin<Pc0>
{
	static inline volatile uint8_t& ddr() { return DDRC; }
	static inline volatile uint8_t& port() { return PORTC; }
	static inline const volatile uint8_t& pin() { return PINC; }
	static inline uint8_t mask() { return 1 << 0; }
};

template<> struct digital_pin<Pc1>
{
	static inline volatile uint8_t& ddr() { return DDRC; }
	static inline volatile uint8_t& port() { return PORTC; }
	static inline const volatile uint8_t& pin() { return PINC; }
	static inline uint8_t mask() { return 1 << 1; }
};

template<> struct digital_pin<Pc2>
{
	static inline volatile uint8_t& ddr() { return DDRC; }
	static inline volatile uint8_t& port() { return PORTC; }
	static inline const volatile uint8_t& pin() { return PINC; }
	static inline uint8_t mask() { return 1 << 2; }
};

template<> struct digital_pin<Pc3>
{
	static inline volatile uint8_t& ddr() { return DDRC; }
	static inline volatile uint8_t& port() { return PORTC; }
	static inline const volatile uint8_t& pin() { return PINC; }
	static inline uint8_t mask() { return 1 << 3; }
};

template<> struct digital_pin<Pc4>
{
	static inline volatile uint8_t& ddr() { return DDRC; }
	static inline volatile uint8_t& port() { return PORTC; }
	static inline const volatile uint8_t& pin() { return PINC; }
	static inline uint8_t mask() { return 1 << 4; }
};

template<> struct digital_pin<Pc5>
{
	static inline volatile uint8_t& ddr() { return DDRC; }
	static inline volatile uint8_t& port() { return PORTC; }
	static inline const volatile uint8_t& pin() { return PINC; }
	static inline volatile uint8_t mask() { return 1 << 5; }
};

template<> struct digital_pin<Pc6>
{
	static inline volatile uint8_t& ddr() { return DDRC; }
	static inline volatile uint8_t& port() { return PORTC; }
	static inline const volatile uint8_t& pin() { return PINC; }
	static inline uint8_t mask() { return 1 << 6; }
};

/* NOT AVAILABLE ON Atmega-328
template<> struct digital_pin<Pc7>
{
	static inline volatile uint8_t& ddr() { return DDRC; }
	static inline volatile uint8_t& port() { return PORTC; }
	static inline const volatile uint8_t& pin() { return PINC; }
	static inline uint8_t mask() { return 1 << 7; }
};
*/

template<> struct digital_pin<Pd0>
{
	static inline volatile uint8_t& ddr() { return DDRD; }
	static inline volatile uint8_t& port() { return PORTD; }
	static inline const volatile uint8_t& pin() { return PIND; }
	static inline uint8_t mask() { return 1 << 0; }
};

template<> struct digital_pin<Pd1>
{
	static inline volatile uint8_t& ddr() { return DDRD; }
	static inline volatile uint8_t& port() { return PORTD; }
	static inline const volatile uint8_t& pin() { return PIND; }
	static inline uint8_t mask() { return 1 << 1; }
};

template<> struct digital_pin<Pd2>
{
	static inline volatile uint8_t& ddr() { return DDRD; }
	static inline volatile uint8_t& port() { return PORTD; }
	static inline const volatile uint8_t& pin() { return PIND; }
	static inline uint8_t mask() { return 1 << 2; }
};

template<> struct digital_pin<Pd3>
{
	static inline volatile uint8_t& ddr() { return DDRD; }
	static inline volatile uint8_t& port() { return PORTD; }
	static inline const volatile uint8_t& pin() { return PIND; }
	static inline uint8_t mask() { return 1 << 3; }
};

template<> struct digital_pin<Pd4>
{
	static inline volatile uint8_t& ddr() { return DDRD; }
	static inline volatile uint8_t& port() { return PORTD; }
	static inline const volatile uint8_t& pin() { return PIND; }
	static inline uint8_t mask() { return 1 << 4; }
};

template<> struct digital_pin<Pd5>
{
	static inline volatile uint8_t& ddr() { return DDRD; }
	static inline volatile uint8_t& port() { return PORTD; }
	static inline const volatile uint8_t& pin() { return PIND; }
	static inline uint8_t mask() { return 1 << 5; }
};

template<> struct digital_pin<Pd6>
{
	static inline volatile uint8_t& ddr() { return DDRD; }
	static inline volatile uint8_t& port() { return PORTD; }
	static inline const volatile uint8_t& pin() { return PIND; }
	static inline uint8_t mask() { return 1 << 6; }
};

template<> struct digital_pin<Pd7>
{
	static inline volatile uint8_t& ddr() { return DDRD; }
	static inline volatile uint8_t& port() { return PORTD; }
	static inline const volatile uint8_t& pin() { return PIND; }
	static inline uint8_t mask() { return 1 << 7; }
};

template<class PIN> static inline void input_mode()
{
	digital_pin<PIN>::ddr() &= ~digital_pin<PIN>::mask();
}

template<class PIN> static inline void output_mode()
{
	digital_pin<PIN>::ddr() |= digital_pin<PIN>::mask();
}

template<class PIN> static inline void set()
{
	digital_pin<PIN>::port() |= digital_pin<PIN>::mask();
}

template<class PIN> static inline void clear()
{
	digital_pin<PIN>::port() &= ~digital_pin<PIN>::mask();
}

template<class PIN> static inline void toggle()
{
	digital_pin<PIN>::port() ^= digital_pin<PIN>::mask();
}

template<class PIN> static inline void write(bool x)
{
	x ? set<PIN>() : clear<PIN>();
}

#endif // PINS_H

