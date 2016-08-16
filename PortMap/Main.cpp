#include <AVR/Main.h>
#include <AVR/Pins.h>
#include <AVR/Delay.h>

typedef output_t<PD, 7> LED;            // uno


template<class PORT, unsigned BIT> struct pint_t : port_t<PORT>
{
    static_assert(BIT < 8, "bit out of range");

    static const int bit = BIT;
    static const uint8_t mask = _BV(BIT);

/*
    static inline void set() { port_t<PORT>::reg() |= mask; }
    static inline void clear() { port_t<PORT>::reg() &= ~mask; }
    static inline void write(bool x) { x ? set() : clear(); }
    static inline void toggle() { port_t<PORT>::reg() ^= mask; }
*/
};

#undef PD0
#undef PD1
#undef PD2
#undef PD3
#undef PD4
#undef PD5
#undef PD6
#undef PD7

typedef pint_t<PD, 0> PD0;
typedef pint_t<PD, 1> PD1;
typedef pint_t<PD, 2> PD2;
typedef pint_t<PD, 3> PD3;
typedef pint_t<PD, 4> PD4;
typedef pint_t<PD, 5> PD5;
typedef pint_t<PD, 6> PD6;
typedef pint_t<PD, 7> PD7;

template<typename PORT, typename PINPORT, int POS, typename PIN>
struct mask_impl
{
    static const uint8_t mask = 0;
    static const int dist = 0;
    static uint8_t shift(uint8_t x) { return 0; }
};

template<typename PORT, int POS, typename PIN>
struct mask_impl<PORT, PORT, POS, PIN>
{
    static const uint8_t mask = PIN::mask;
    static const int dist = PIN::bit - POS;
    static uint8_t shift(uint8_t x) { return ::shift<dist>(static_cast<uint8_t>(x & _BV(POS))); }
};

static_assert(mask_impl<PD, PD, 2, PD6>::dist == 4, "");
static_assert(mask_impl<PD, PD, 7, PD5>::dist == -2, "");
static_assert(mask_impl<PC, PD, 2, PD6>::dist == 0, "");

template<typename PORT, int POS, typename...PINS>
struct mask_t;

template<typename PORT, int POS, typename PIN>
struct mask_t<PORT, POS, PIN>
{
    static const uint8_t mask = mask_impl<PORT, typename PIN::port, POS, PIN>::mask;
    static uint8_t shift(uint8_t x) { return mask_impl<PORT, typename PIN::port, POS, PIN>::shift(x); }
};

template<typename PORT, int POS, typename PIN, typename...TAIL>
struct mask_t<PORT, POS, PIN, TAIL...>
{
    static const uint8_t mask = mask_t<PORT, POS, PIN>::mask | mask_t<PORT, POS + 1, TAIL...>::mask;
    static uint8_t shift(uint8_t x) { return mask_t<PORT, POS, PIN>::shift(x) | mask_t<PORT, POS + 1, TAIL...>::shift(x); }
};

static_assert(mask_t<PD, 0, PD0>::mask == 0x01, "");
static_assert(mask_t<PD, 0, PD0, PD1>::mask == 0x03, "");
static_assert(mask_t<PD, 0, PD0, PD1, PD2>::mask == 0x07, "");
static_assert(mask_t<PC, 0, PD0, PD1, PD2>::mask == 0x00, "");


template<typename...PINS>
struct portmap_t
{
    static const uint8_t mask = mask_t<PD, 0, PINS...>::mask;

    static inline void set(uint8_t x)
    {
        PORTB = mask;
    }

};

/*
template<typename PIN, typename...TAIL>
struct portmap_t<PIN, TAIL...>
{
    static const uint8_t mask = 0;

    static inline void setup()
    {
    }

    static inline void set(uint8_t x)
    {
        PORTB = mask;
    }
};

*/

typedef portmap_t<PD7> w;

void setup()
{
    LED::setup();
}

void loop()
{
    LED::toggle();
    w::set(0xff);
    delay_ms(100);
}

