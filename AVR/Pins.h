#ifndef PINS_H
#define PINS_H

template<class PORT> struct port_t
{
	static inline volatile uint8_t& ddr();
	static inline volatile uint8_t& port();
	static inline const volatile uint8_t& pin();
};

template<class PIN> struct pin_t
{
	static inline uint8_t mask();
};

struct PortB;;
struct PortC;;
struct PortD;;

template<> struct port_t<PortB>
{
	static inline volatile uint8_t& ddr() { return DDRB; }
	static inline volatile uint8_t& port() { return PORTB; }
	static inline volatile const uint8_t& pin() { return PINB; }
};

template<> struct port_t<PortC>
{
	static inline volatile uint8_t& ddr() { return DDRC; }
	static inline volatile uint8_t& port() { return PORTC; }
	static inline volatile const uint8_t& pin() { return PINC; }
};

template<> struct port_t<PortD>
{
	static inline volatile uint8_t& ddr() { return DDRD; }
	static inline volatile uint8_t& port() { return PORTD; }
	static inline volatile const uint8_t& pin() { return PIND; }
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

template<> struct pin_t<Pb0> : port_t<PortB> { static inline uint8_t mask() { return 1 << 0; } };
template<> struct pin_t<Pb1> : port_t<PortB> { static inline uint8_t mask() { return 1 << 1; } };
template<> struct pin_t<Pb2> : port_t<PortB> { static inline uint8_t mask() { return 1 << 2; } };
template<> struct pin_t<Pb3> : port_t<PortB> { static inline uint8_t mask() { return 1 << 3; } };
template<> struct pin_t<Pb4> : port_t<PortB> { static inline uint8_t mask() { return 1 << 4; } };
template<> struct pin_t<Pb5> : port_t<PortB> { static inline uint8_t mask() { return 1 << 5; } };
/* NOT AVAILABLE ON Atmega-328 (used for crystal)
template<> struct pin_t<Pb6> : port_t<PortB> { static inline uint8_t mask() { return 1 << 6; } };
template<> struct pin_t<Pb7> : port_t<PortB> { static inline uint8_t mask() { return 1 << 7; } };
*/

template<> struct pin_t<Pc0> : port_t<PortC> { static inline uint8_t mask() { return 1 << 0; } };
template<> struct pin_t<Pc1> : port_t<PortC> { static inline uint8_t mask() { return 1 << 1; } };
template<> struct pin_t<Pc2> : port_t<PortC> { static inline uint8_t mask() { return 1 << 2; } };
template<> struct pin_t<Pc3> : port_t<PortC> { static inline uint8_t mask() { return 1 << 3; } };
template<> struct pin_t<Pc4> : port_t<PortC> { static inline uint8_t mask() { return 1 << 4; } };
template<> struct pin_t<Pc5> : port_t<PortC> { static inline uint8_t mask() { return 1 << 5; } };
template<> struct pin_t<Pc6> : port_t<PortC> { static inline uint8_t mask() { return 1 << 6; } };
/* NOT AVAILABLE ON Atmega-328
template<> struct pin_t<Pc7> : port_t<PortC> { static inline uint8_t mask() { return 1 << 7; } };
*/

template<> struct pin_t<Pd0> : port_t<PortD> { static inline uint8_t mask() { return 1 << 0; } };
template<> struct pin_t<Pd1> : port_t<PortD> { static inline uint8_t mask() { return 1 << 1; } };
template<> struct pin_t<Pd2> : port_t<PortD> { static inline uint8_t mask() { return 1 << 2; } };
template<> struct pin_t<Pd3> : port_t<PortD> { static inline uint8_t mask() { return 1 << 3; } };
template<> struct pin_t<Pd4> : port_t<PortD> { static inline uint8_t mask() { return 1 << 4; } };
template<> struct pin_t<Pd5> : port_t<PortD> { static inline uint8_t mask() { return 1 << 5; } };
template<> struct pin_t<Pd6> : port_t<PortD> { static inline uint8_t mask() { return 1 << 6; } };
template<> struct pin_t<Pd7> : port_t<PortD> { static inline uint8_t mask() { return 1 << 7; } };

template<class PIN> static inline void input_mode()
{
	pin_t<PIN>::ddr() &= ~pin_t<PIN>::mask();
}

template<class PIN> static inline void output_mode()
{
	pin_t<PIN>::ddr() |= pin_t<PIN>::mask();
}

template<class PIN> static inline void set()
{
	pin_t<PIN>::port() |= pin_t<PIN>::mask();
}

template<class PIN> static inline void clear()
{
	pin_t<PIN>::port() &= ~pin_t<PIN>::mask();
}

template<class PIN> static inline void toggle()
{
	pin_t<PIN>::port() ^= pin_t<PIN>::mask();
}

template<class PIN> static inline void write(bool x)
{
	x ? set<PIN>() : clear<PIN>();
}

#endif // PINS_H

