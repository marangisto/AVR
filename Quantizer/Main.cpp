#define NO_TIMER_VECTORS 1
#include <AVR/Main.h>
#include <AVR/ADC.h>
#include <AVR/SPI.h>
#include <AVR/MCP48x2.h>
#include <AVR/Delay.h>
#include <AVR/Timer.h>
#include <AVR/Button.h>

template<class T> T max(const T& x, const T& y) { return x > y ? x : y; }
template<class T> T min(const T& x, const T& y) { return x < y ? x : y; }

typedef output_t<PC, 3> led;
typedef spi_t<2, msb_first, PB, 2> spi;
typedef output_t<PB, 1> dac;
typedef button_t<PD, 6> btn_dn;
typedef button_t<PD, 7> btn_up;

static const uint8_t adc_offset = 0;
static const uint8_t adc_cva = 2;
static const uint8_t adc_cvb = 1;

typedef timer_t<0> debounce;

ISR(TIMER0_OVF_vect)
{
    btn_up::update();
    btn_dn::update();
}

static const uint16_t chromatic_tab[] =
    { 0,83,167,250,333,417,500,583,667,750,833,917,1000,1083,1167,1250
    , 1333,1417,1500,1583,1667,1750,1833,1917,2000,2083,2167,2250,2333,2417,2500,2583
    , 2667,2750,2833,2917,3000,3083,3167,3250,3333,3417,3500,3583,3667,3750,3833,3917
    , 4000,4083
    };

static const uint8_t chromatic_tab_size = sizeof(chromatic_tab) / sizeof(*chromatic_tab);

static inline uint8_t find_index(uint16_t cv)
{
    uint8_t i = min<uint16_t>(cv >> 6, chromatic_tab_size - 1);

    while (i < chromatic_tab_size - 1 && chromatic_tab[i] < cv)
        ++i;
    while (i > 0 && chromatic_tab[i] > cv)
        --i;
    return i;
}

void setup()
{
    adc::setup<128>();
    led::setup();
    spi::setup();
    dac::setup();
    dac::set();
    btn_up::setup();
    btn_dn::setup();
    debounce::template setup<normal_mode>();
    debounce::template clock_select<64>();
    debounce::enable();
    sei();
}

void loop()
{
    static uint16_t i = 0;
    static uint16_t last_cva = 0, last_cvb = 0;
    uint16_t j = adc::read<adc_offset>();
    uint16_t cva = (static_cast<uint32_t>(adc::read<adc_cva>() << 2) << 10) / 819;    // FIXME: use input stage scaling!
    uint16_t cvb = (static_cast<uint32_t>(adc::read<adc_cvb>() << 2) << 10) / 819;    // FIXME: use input stage scaling!
    uint16_t ot = j << 2;

    if (btn_up::read())
        i = (i + 1) & 0x0fff;

    if (btn_dn::read())
        i = (i - 1) & 0x0fff;

    cva = chromatic_tab[find_index(cva)];
    cvb = chromatic_tab[find_index(cvb)];
    spi::write(mcp48x2_t::encode<chan_a, gain_x2>(cva));
    spi::write(mcp48x2_t::encode<chan_b, gain_x2>(ot+cvb));

    dac::clear();
    dac::set();

    if (cva != last_cva || cvb != last_cvb)
    {
        led::toggle();
        last_cva = cva;
        last_cvb = cvb;
    }
}

