#include <kinetis.h>

extern "C" void delay(uint32_t ms); // FIXME: figure out where this comes from

typedef uint32_t word_t;                            // port word type

enum port_enum_t { P_, PA, PB, PC, PD, PE };

template<port_enum_t PORT> struct port_t
{
    template<uint8_t BIT>
    static inline volatile word_t& pcr0();          // pin control register base
    static inline volatile word_t& pddr();          // data direction register
    static inline volatile word_t& pdor();          // data output register
    static inline volatile word_t& psor();          // set output register
    static inline volatile word_t& pcor();          // clear output register
    static inline volatile word_t& ptor();          // toggle output register
    static inline const volatile word_t& pdir();    // data input register
};

template<> struct port_t<PA>
{
    static const port_enum_t port = PA;
    static inline volatile word_t& pcr0() { return PORTA_PCR0; }
    static inline volatile word_t& pddr() { return GPIOA_PDDR; }
    static inline volatile word_t& pdor() { return GPIOA_PDOR; }
    static inline volatile word_t& psor() { return GPIOA_PSOR; }
    static inline volatile word_t& pcor() { return GPIOA_PCOR; }
    static inline volatile word_t& ptor() { return GPIOA_PTOR; }
    static inline const volatile word_t& pdir() { return GPIOA_PDIR; }
};

template<> struct port_t<PB>
{
    static const port_enum_t port = PB;
    static inline volatile word_t& pcr0() { return PORTB_PCR0; }
    static inline volatile word_t& pddr() { return GPIOB_PDDR; }
    static inline volatile word_t& pdor() { return GPIOB_PDOR; }
    static inline volatile word_t& psor() { return GPIOB_PSOR; }
    static inline volatile word_t& pcor() { return GPIOB_PCOR; }
    static inline volatile word_t& ptor() { return GPIOB_PTOR; }
    static inline const volatile word_t& pdir() { return GPIOB_PDIR; }
};

template<> struct port_t<PC>
{
    static const port_enum_t port = PC;
    static inline volatile word_t& pcr0() { return PORTC_PCR0; }
    static inline volatile word_t& pddr() { return GPIOC_PDDR; }
    static inline volatile word_t& pdor() { return GPIOC_PDOR; }
    static inline volatile word_t& psor() { return GPIOC_PSOR; }
    static inline volatile word_t& pcor() { return GPIOC_PCOR; }
    static inline volatile word_t& ptor() { return GPIOC_PTOR; }
    static inline const volatile word_t& pdir() { return GPIOC_PDIR; }
};

template<> struct port_t<PD>
{
    static const port_enum_t port = PD;
    static inline volatile word_t& pcr0() { return PORTD_PCR0; }
    static inline volatile word_t& pddr() { return GPIOD_PDDR; }
    static inline volatile word_t& pdor() { return GPIOD_PDOR; }
    static inline volatile word_t& psor() { return GPIOD_PSOR; }
    static inline volatile word_t& pcor() { return GPIOD_PCOR; }
    static inline volatile word_t& ptor() { return GPIOD_PTOR; }
    static inline const volatile word_t& pdir() { return GPIOD_PDIR; }
};

template<> struct port_t<PE>
{
    static const port_enum_t port = PE;
    static inline volatile word_t& pcr0() { return PORTE_PCR0; }
    static inline volatile word_t& pddr() { return GPIOE_PDDR; }
    static inline volatile word_t& pdor() { return GPIOE_PDOR; }
    static inline volatile word_t& psor() { return GPIOE_PSOR; }
    static inline volatile word_t& pcor() { return GPIOE_PCOR; }
    static inline volatile word_t& ptor() { return GPIOE_PTOR; }
    static inline const volatile word_t& pdir() { return GPIOE_PDIR; }
};

template<port_enum_t PORT, uint8_t BIT> struct pin_t
{
    typedef port_t<PORT> port;
    static_assert(BIT < 8 * sizeof(word_t), "bit out of range");
    static const uint8_t bitpos = BIT;
    static const word_t bitmask = 1<<BIT;
    static inline volatile word_t& pcr() { return *(&port::pcr0() + BIT); }
};

enum pullup_t { nopull, pullup, pulldown };

template<pullup_t> struct pullup_traits;
template<> struct pullup_traits<nopull> { static const word_t flags = 0; };
template<> struct pullup_traits<pullup> { static const word_t flags = PORT_PCR_PE | PORT_PCR_PS; };
template<> struct pullup_traits<pulldown> { static const word_t flags = PORT_PCR_PE; };

template<class PIN> struct input_t
{
    template<pullup_t PULLUP = nopull>
    static inline void setup()
    {
        PIN::port::pddr() &= ~PIN::bitmask;
        PIN::pcr() = PORT_PCR_LK | PORT_PCR_MUX(1) | pullup_traits<PULLUP>::flags;
    }

    static inline bool get() { return (PIN::port::pdir() & PIN::bitmask) != 0; }
};

enum drain_t { common_drain, open_drain };

template<drain_t> struct drain_traits;
template<> struct drain_traits<common_drain> { static const word_t flags = 0; };
template<> struct drain_traits<open_drain> { static const word_t flags = PORT_PCR_ODE; };

template<class PIN> struct output_t
{
    template<drain_t DRAIN = common_drain>
    static inline void setup()
    {
        PIN::port::pddr() |= PIN::bitmask;
        PIN::pcr() = PORT_PCR_LK | PORT_PCR_MUX(1) | PORT_PCR_SRE | PORT_PCR_DSE | drain_traits<DRAIN>::flags;
    }

    static inline bool get() { return (PIN::port::pdor() & PIN::bitmask) != 0; }
    static inline void set(bool x) { x ? set() : clear(); }
    static inline void set() { PIN::port::psor() = PIN::bitmask; }
    static inline void clear() { PIN::port::pcor() = PIN::bitmask; }
    static inline void toggle() { PIN::port::ptor() = PIN::bitmask; }
};

typedef pin_t<PB, 16> P0;
typedef pin_t<PB, 17> P1;
typedef pin_t<PD,  0> P2;
typedef pin_t<PA, 12> P3;
typedef pin_t<PA, 13> P4;
typedef pin_t<PD,  7> P5;
typedef pin_t<PD,  4> P6;
typedef pin_t<PD,  2> P7;
typedef pin_t<PD,  3> P8;
typedef pin_t<PC,  3> P9;
typedef pin_t<PC,  4> P10;
typedef pin_t<PC,  6> P11;
typedef pin_t<PC,  7> P12;
typedef pin_t<PC,  5> P13;
typedef pin_t<PD,  1> P14;
typedef pin_t<PC,  0> P15;
typedef pin_t<PB,  0> P16;
typedef pin_t<PB,  1> P17;
typedef pin_t<PB,  3> P18;
typedef pin_t<PB,  2> P19;
typedef pin_t<PD,  5> P20;
typedef pin_t<PD,  6> P21;
typedef pin_t<PC,  1> P22;
typedef pin_t<PC,  2> P23;
typedef pin_t<PE, 26> P24;
typedef pin_t<PA,  5> P25;
typedef pin_t<PA, 14> P26;
typedef pin_t<PA, 15> P27;
typedef pin_t<PA, 16> P28;
typedef pin_t<PB, 18> P29;
typedef pin_t<PB, 19> P30;
typedef pin_t<PB, 10> P31;
typedef pin_t<PB, 11> P32;
typedef pin_t<PE, 24> P33;
typedef pin_t<PE, 25> P34;
typedef pin_t<PC,  8> P35;
typedef pin_t<PC,  9> P36;
typedef pin_t<PC, 10> P37;
typedef pin_t<PC, 11> P38;
typedef pin_t<PA, 17> P39;
typedef pin_t<PA, 28> P40;
typedef pin_t<PA, 29> P41;
typedef pin_t<PA, 26> P42;
typedef pin_t<PB, 20> P43;
typedef pin_t<PB, 22> P44;
typedef pin_t<PB, 23> P45;
typedef pin_t<PB, 21> P46;
typedef pin_t<PD,  8> P47;
typedef pin_t<PD,  9> P48;
typedef pin_t<PB,  4> P49;
typedef pin_t<PB,  5> P50;
typedef pin_t<PD, 14> P51;
typedef pin_t<PD, 13> P52;
typedef pin_t<PD, 12> P53;
typedef pin_t<PD, 15> P54;
typedef pin_t<PD, 11> P55;
typedef pin_t<PE, 10> P56;
typedef pin_t<PE, 11> P57;
typedef pin_t<PE,  0> P58;
typedef pin_t<PE,  1> P59;
typedef pin_t<PE,  2> P60;
typedef pin_t<PE,  3> P61;
typedef pin_t<PE,  4> P62;
typedef pin_t<PE,  5> P63;

// multi-pin stuff

template<int N, bool IS_NEGATIVE>
struct shift_impl { static uint8_t shift(uint8_t x) { return x << N; } };

template<int N>
struct shift_impl<N, true> { static uint8_t shift(uint8_t x) { return x >> -N; } };

template<int N>
uint8_t shift(uint8_t x) { return shift_impl<N, N < 0>::shift(x); }

template<port_enum_t PORT, port_enum_t PINPORT, int POS, typename PIN>
struct mask_impl
{
    static const uint8_t mask = 0;
    static const int dist = 0;
    static uint8_t shift(uint8_t x) { return 0; }
};

template<port_enum_t PORT, int POS, typename PIN>
struct mask_impl<PORT, PORT, POS, PIN>
{
    static const uint8_t mask = PIN::bitmask;
    static const int dist = PIN::bitpos - POS;
    static uint8_t shift(uint8_t x) { return ::shift<dist>(static_cast<uint8_t>(x & (1<<POS))); }
};

static_assert(mask_impl<PD, PD, 2, P51>::dist == 12, "");
static_assert(mask_impl<PB, PB, 7, P19>::dist == -5, "");
static_assert(mask_impl<PC, PC, 7, P12>::dist == 0, "");
static_assert(mask_impl<PA, PD, 2, P51>::dist == 0, "");    // wrong port

template<port_enum_t PORT, int POS, typename...PINS>
struct mask_t;

template<port_enum_t PORT, int POS, typename PIN>
struct mask_t<PORT, POS, PIN>
{
    static const uint8_t mask = mask_impl<PORT, PIN::port::port, POS, PIN>::mask;
    static uint8_t shift(uint8_t x) { return mask_impl<PORT, PIN::port::port, POS, PIN>::shift(x); }
};

template<port_enum_t PORT, int POS, typename PIN, typename...TAIL>
struct mask_t<PORT, POS, PIN, TAIL...>
{
    static const uint8_t mask = mask_t<PORT, POS, PIN>::mask | mask_t<PORT, POS + 1, TAIL...>::mask;
    static uint8_t shift(uint8_t x) { return mask_t<PORT, POS, PIN>::shift(x) | mask_t<PORT, POS + 1, TAIL...>::shift(x); }
};

static_assert(mask_t<PB, 0, P16>::mask == 0x01, "");
static_assert(mask_t<PB, 0, P16, P17>::mask == 0x03, "");
static_assert(mask_t<PB, 0, P17, P16>::mask == 0x03, "");
static_assert(mask_t<PB, 0, P16, P17, P19>::mask == 0x07, "");
static_assert(mask_t<PC, 0, P16, P17, P19>::mask == 0x00, "");

template<typename...PINS>
struct outputs_t
{
    template<drain_t DRAIN = common_drain>
    static inline void setup()
    {
	// FIXME: initialize pin settings here
    }

    static inline uint8_t get() { return 0; }
    static inline void set(uint8_t x) {}
};

// demo program

typedef output_t<P13> LED;
typedef output_t<P14> LED2;
typedef output_t<P15> LED3;
typedef input_t<P16> BTN;


typedef outputs_t<P17, P18, P19> W3;

extern "C"
void setup()
{
    LED::setup();
    LED2::setup<common_drain>();
    LED3::setup();
    BTN::setup<pullup>();
    W3::setup();
}

extern "C"
void loop()
{
    static uint8_t i = 0;
    if (BTN::get())
        LED::toggle();
    if (!LED::get())
    {
        LED2::toggle();
        if (!LED2::get())
            LED3::toggle();
    }
    W3::set(i++);
    delay(100);
}

