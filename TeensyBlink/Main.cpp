#include <kinetis.h>

extern "C" void delay(uint32_t ms); // FIXME: figure out where this comes from

// N.B. PORTC-PIN5 == LED 'Pin 13'

typedef uint32_t word_t;                            // port word type

struct P_;                                          // not a port
struct PA;
struct PB;
struct PC;
struct PD;
struct PE;

template<class PORT> struct port_t
{
    static inline volatile word_t& pddr();          // data direction register
    static inline volatile word_t& pdor();          // data output register
    static inline volatile word_t& psor();          // set output register
    static inline volatile word_t& pcor();          // clear output register
    static inline volatile word_t& ptor();          // toggle output register
    static inline const volatile word_t& pdir();    // data input register
};

template<> struct port_t<PA>
{
    typedef PA PORT;
    static inline volatile word_t& pddr() { return GPIOA_PDDR; }
    static inline volatile word_t& pdor() { return GPIOA_PDOR; }
    static inline volatile word_t& psor() { return GPIOA_PSOR; }
    static inline volatile word_t& pcor() { return GPIOA_PCOR; }
    static inline volatile word_t& ptor() { return GPIOA_PTOR; }
    static inline const volatile word_t& pdir() { return GPIOA_PDIR; }
};

template<> struct port_t<PB>
{
    typedef PB PORT;
    static inline volatile word_t& pddr() { return GPIOB_PDDR; }
    static inline volatile word_t& pdor() { return GPIOB_PDOR; }
    static inline volatile word_t& psor() { return GPIOB_PSOR; }
    static inline volatile word_t& pcor() { return GPIOB_PCOR; }
    static inline volatile word_t& ptor() { return GPIOB_PTOR; }
    static inline const volatile word_t& pdir() { return GPIOB_PDIR; }
};

template<> struct port_t<PC>
{
    typedef PC PORT;
    static inline volatile word_t& pddr() { return GPIOC_PDDR; }
    static inline volatile word_t& pdor() { return GPIOC_PDOR; }
    static inline volatile word_t& psor() { return GPIOC_PSOR; }
    static inline volatile word_t& pcor() { return GPIOC_PCOR; }
    static inline volatile word_t& ptor() { return GPIOC_PTOR; }
    static inline const volatile word_t& pdir() { return GPIOC_PDIR; }
};

template<> struct port_t<PD>
{
    typedef PD PORT;
    static inline volatile word_t& pddr() { return GPIOD_PDDR; }
    static inline volatile word_t& pdor() { return GPIOD_PDOR; }
    static inline volatile word_t& psor() { return GPIOD_PSOR; }
    static inline volatile word_t& pcor() { return GPIOD_PCOR; }
    static inline volatile word_t& ptor() { return GPIOD_PTOR; }
    static inline const volatile word_t& pdir() { return GPIOD_PDIR; }
};

template<> struct port_t<PE>
{
    typedef PE PORT;
    static inline volatile word_t& pddr() { return GPIOE_PDDR; }
    static inline volatile word_t& pdor() { return GPIOE_PDOR; }
    static inline volatile word_t& psor() { return GPIOE_PSOR; }
    static inline volatile word_t& pcor() { return GPIOE_PCOR; }
    static inline volatile word_t& ptor() { return GPIOE_PTOR; }
    static inline const volatile word_t& pdir() { return GPIOE_PDIR; }
};

template<class PORT, uint8_t BIT> struct pin_control_register;

template<uint8_t BIT> struct pin_control_register<PA, BIT> { static inline volatile word_t& reg() { return *(&PORTA_PCR0 + BIT); } };
template<uint8_t BIT> struct pin_control_register<PB, BIT> { static inline volatile word_t& reg() { return *(&PORTB_PCR0 + BIT); } };
template<uint8_t BIT> struct pin_control_register<PC, BIT> { static inline volatile word_t& reg() { return *(&PORTC_PCR0 + BIT); } };
template<uint8_t BIT> struct pin_control_register<PD, BIT> { static inline volatile word_t& reg() { return *(&PORTD_PCR0 + BIT); } };
template<uint8_t BIT> struct pin_control_register<PE, BIT> { static inline volatile word_t& reg() { return *(&PORTE_PCR0 + BIT); } };

template<class PORT, uint8_t BIT> struct pin_t : port_t<PORT>
{
    static_assert(BIT < 8 * sizeof(word_t), "bit out of range");
    static const uint8_t bitpos = BIT;
    static const word_t bitmask = 1<<BIT;
    static inline volatile word_t& pcr() { return pin_control_register<PORT, BIT>::reg(); }
};

typedef pin_t<PC, 5> LED;
typedef pin_t<PA, 0> LED2;
// define all pins here

extern "C"
void setup()
{
    LED::pddr() |= LED::bitmask;
    LED::pcr() = PORT_PCR_SRE | PORT_PCR_DSE | PORT_PCR_MUX(1);
    LED2::pcr() = PORT_PCR_SRE | PORT_PCR_DSE | PORT_PCR_MUX(1);
    //GPIOC_PDDR |= (1<<5);
    //PORTC_PCR5 = PORT_PCR_SRE | PORT_PCR_DSE | PORT_PCR_MUX(1);
}

extern "C"
void loop()
{
    //GPIOC_PSOR = (1<<5);  // SET
    //delay(50);

    //GPIOC_PCOR = (1<<5);  // CLEAR

    //GPIOC_PTOR = (1<<5);    // TOGGLE
    LED::ptor() = LED::bitmask;
    delay(500);
}

