#define NO_TIMER_VECTORS 1
#define USE_UART 0
#if USE_UART
#include <AVR/UART.h>
#endif
#include <AVR/TWI.h>
#include <AVR/ADC.h>
#include <AVR/Main.h>
#include <AVR/Delay.h>
#include <AVR/Timer.h>
#include <AVR/Pins.h>
#include <stdlib.h>

template <class T> const T& max(const T& a, const T& b) { return (a<b) ? b : a; }
template <class T> const T& min(const T& a, const T& b) { return (a<b) ? a : b; }

typedef input_t<PD, 3> clk_a;
typedef input_t<PD, 4> rst_a;
typedef input_t<PD, 5> clk_b;
typedef input_t<PD, 6> rst_b;

typedef output_t<PB, 0> trig_a1;
typedef output_t<PB, 3> trig_a2;
typedef output_t<PB, 4> trig_b1;
typedef output_t<PB, 5> trig_b2;

typedef output_t<PE, 0> scan0;
typedef output_t<PE, 1> scan1;
typedef output_t<PE, 2> scan2;
typedef output_t<PE, 3> scan3;

typedef input_t<PD, 1> sense0;
typedef input_t<PB, 7> sense1;
typedef input_t<PD, 7> sense2;

static const uint8_t adc_start_a = 0;   // PC0
static const uint8_t adc_finish_a = 1;  // PC1
static const uint8_t adc_start_b = 2;   // PC2
static const uint8_t adc_finish_b = 3;  // PC3

typedef twi_master_t<0> twi;

typedef timer_t<3> pwm_a;     // channel-a (cv1, cv2)
typedef timer_t<1> pwm_b;     // channel-b (cv1, cv2)

static const uint8_t max_subseqs = 8;
static uint8_t twi_addr[max_subseqs];
static volatile uint8_t led_state[max_subseqs];
static uint8_t n_subseqs = 0;

ISR(TWI0_vect)
{
    twi::isr();
}

static void attach_subseqs()
{
    n_subseqs = 0;

    for (uint8_t a = 0; a < 128; ++a)
    {
        uint8_t buf[1];

        if ((a & 0x78) == 0 || (a & 0x78) == 0x78)
            continue;   // reserved address
        if (twi::write(a, buf, 0) == 0 && n_subseqs < max_subseqs)
        {
            led_state[n_subseqs] = 0;
            twi_addr[n_subseqs++] = a;
#if USE_UART
            printf("found sub-sequence at 0x%02x\n", a);
#endif
        }
    }
}

enum state_t { STOPPED, RUNNING };
enum mode_t { NORMAL, PENDULUM, RANDOM };

template<class CLK, class RST, class TRIG_A, class TRIG_B, class PWM>
class channel_t
{
public:
    void init(uint8_t nsteps)
    {
        m_nsteps = nsteps;
        m_state = STOPPED;
        m_mode = NORMAL;
        m_step = 0;
        m_start = 0;
        m_finish = nsteps - 1;
    }

    inline void start() { m_state = RUNNING; }
    inline void stop() { m_state = STOPPED; }

    inline void reset()
    {
        m_step = m_finish;  // next clock will be start step
        m_tock = false;     // pemdulum state variable
    }

    inline state_t state() const { return m_state; }
    inline uint8_t step() const { return m_step; }

    inline void set_level(uint8_t i, uint16_t x)
    {
        if (i < sizeof(m_level) / sizeof(*m_level))
            m_level[i] = x;
    }

    inline void advance()
    {
        uint8_t lo = min(m_start, m_finish), hi = max(m_start, m_finish);
        bool fwd = m_tock ^ (m_start < m_finish);

        if (m_mode == RANDOM)
            m_step = hi > lo ? lo + rand() % (hi - lo) : lo;
        else if (fwd)
            m_step = m_step < hi ? m_step + 1 : lo;
        else
            m_step = m_step > lo ? m_step - 1 : hi;

        if (m_mode == PENDULUM && (m_step == hi || m_step == lo))
            m_tock = !m_tock;
    }

    inline void jog(bool rev)
    {
        cli();

        if (rev)
        {
            if (m_finish > m_start)
                m_step = m_step > m_start ? m_step - 1 : m_finish;
            else
                m_step = m_step < m_start ? m_step + 1 : m_finish;
        }
        else
        {
            if (m_finish > m_start)
                m_step = m_step < m_finish ? m_step + 1 : m_start;
            else
                m_step = m_step > m_finish ? m_step - 1 : m_start;
        }

        sei();
    }

    inline void fire(bool up)
    {
        if (m_state != STOPPED)
            return;

        if (up)
        {
            uint16_t x = m_level[m_step];   // including switch bits (shift 1 bit right for 9-bit DAC)

            if ((x & (1 << 13)) != 0)
            {
                PWM::template output_compare_register<channel_a>() = x >> 1;
                TRIG_A::set();
            }
            if ((x & (1 << 14)) != 0)
            {
                PWM::template output_compare_register<channel_b>() = x >> 1;
                TRIG_B::set();
            }
        }
        else
        {
            TRIG_A::clear();
            TRIG_B::clear();
        }
    }

    inline void isr()
    {
        static bool last_clk;

        if (!RST::read())
            reset();

        bool clk = !CLK::read();

        if (clk != last_clk)                    // only process edges
        {
            if (clk && m_state == RUNNING)
            {
                advance();

                uint16_t x = m_level[m_step];   // including switch bits (shift 1 bit right for 9-bit DAC)

                if ((x & (1 << 13)) != 0)
                {
                    PWM::template output_compare_register<channel_a>() = x >> 1;
                    TRIG_A::set();
                }
                if ((x & (1 << 14)) != 0)
                {
                    PWM::template output_compare_register<channel_b>() = x >> 1;
                    TRIG_B::set();
                }
            }
            else
            {
                TRIG_A::clear();
                TRIG_B::clear();
            }
            last_clk = clk;
        }
    }

    inline void set_start(uint8_t i) { m_start = i; }
    inline void set_finish(uint8_t i) { m_finish = i; }
    inline void set_mode(mode_t m) { m_mode = m; }

private:
    volatile uint8_t    m_nsteps;
    volatile state_t    m_state;
    volatile mode_t     m_mode;
    volatile bool       m_tock;
    volatile uint8_t    m_step;
    volatile uint8_t    m_start;
    volatile uint8_t    m_finish;
    volatile uint16_t   m_level[max_subseqs * 4];
};

static channel_t<clk_a, rst_a, trig_a1, trig_a2, pwm_a> ch_a;
static channel_t<clk_b, rst_b, trig_b1, trig_b2, pwm_b> ch_b;

ISR(PCINT2_vect)
{
    ch_a.isr();
    ch_b.isr();
}

// We have 3 sense lines and 4 scan lines. We encode the 3
// sense inputs into 3 adjacent bits and shift these left
// for each scan increment. Note the reads are inverted.

static const uint16_t sw_run_a = (1 << 9);   // run state a
static const uint16_t sw_rst_a = (1 << 1);   // reset a
static const uint16_t sw_rnd_a = (1 << 0);   // a mode swith up
static const uint16_t sw_pnd_a = (1 << 4);   // a mode swith down

static const uint16_t sw_run_b = (1 << 3);   // run state b
static const uint16_t sw_rst_b = (1 << 7);   // reset b
static const uint16_t sw_rnd_b = (1 << 6);   // b mode switch up
static const uint16_t sw_pnd_b = (1 << 10);  // b mode switch down

static const uint16_t sw_side = (1 << 8);    // manual action side
static const uint16_t sw_inc = (1 << 5);     // increment step
static const uint16_t sw_dec = (1 << 2);     // decrement step
static const uint16_t sw_trg = (1 << 11);    // manual trigger

static uint16_t scan_switches()
{
    typedef outputs_t<scan3, scan2, scan1, scan0> scan;

    uint16_t x = 0;

    for (uint8_t i = 0; i < 4; ++i)
    {
        scan::write(~(1 << i));
        x = (x << 3)
          | (sense0::read() ? 0 : 0x01)
          | (sense1::read() ? 0 : 0x02)
          | (sense2::read() ? 0 : 0x04)
          ;
    }

    return x;
}

void setup()
{
#if USE_UART
    UART::setup<115200>();
#endif
    adc::setup<128>();

    rst_a::setup();
    clk_b::setup();
    rst_b::setup();

    trig_a1::setup();
    trig_a2::setup();
    trig_b1::setup();
    trig_b2::setup();

    sense0::setup();
    sense1::setup();
    sense2::setup();

    scan0::setup();
    scan1::setup();
    scan2::setup();
    scan3::setup();

    pwm_a::setup<fast_pwm, top_0x1ff>();
    pwm_a::clock_select<1>();
    pwm_a::output_pin<channel_a>::setup();
    pwm_a::output_pin<channel_b>::setup();
    pwm_a::output_pin<channel_b>::set();    // required for OC3B!
    pwm_a::compare_output_mode<channel_a, clear_on_compare_match>();
    pwm_a::compare_output_mode<channel_b, clear_on_compare_match>();

    pwm_b::setup<fast_pwm, top_0x1ff>();
    pwm_b::clock_select<1>();
    pwm_b::output_pin<channel_a>::setup();
    pwm_b::output_pin<channel_b>::setup();
    pwm_b::compare_output_mode<channel_a, clear_on_compare_match>();
    pwm_b::compare_output_mode<channel_b, clear_on_compare_match>();

    twi::setup(100000);
    sei();

#if USE_UART
    printf("Marangisto Modular Sequencer, V1.0\n");
#endif

    attach_subseqs();

    ch_a.init(16);  // FIXME: use sub-sequences
    ch_b.init(16);  // FIXME: use sub-sequences

    PCMSK2 |= _BV(PCINT19) | _BV(PCINT20) | _BV(PCINT21) | _BV(PCINT22); // PCI for clk[1,2] + rst[1,2]
    PCICR |= _BV(PCIE2);    // enable channel 2 pin-change interrupts

    static uint16_t xs[] = { 0, 68, 136, 204, 272, 340, 408, 476, 544, 612, 680, 748, 816, 884, 952, 1023 };

    for (uint8_t i = 0; i < sizeof(xs) / sizeof(*xs); ++i)
    {
        ch_a.set_level(i, xs[i]);
        ch_b.set_level(i, xs[i]);
    }
}

void loop()
{
    // read switches

    static uint16_t last_sw = 0;

    uint16_t sw = scan_switches(), tmp = 0;

    if (sw != last_sw)
    {
        static uint16_t last_state_a = 0;
        static uint16_t last_mode_a = 0;
        static uint16_t last_state_b = 0;
        static uint16_t last_mode_b = 0;
        static uint16_t last_jog = 0;
        static uint16_t last_trg = 0;
 
        if ((tmp = (sw & (sw_run_a | sw_rst_a))) != last_state_a)
        {
            if (tmp & sw_run_a)
                ch_a.start();
            else if (tmp & sw_rst_a)
                ch_a.reset();
            else
                ch_a.stop();
            last_state_a = tmp;
        }

        if ((tmp = (sw & (sw_run_b | sw_rst_b))) != last_state_b)
        {
            if (tmp & sw_run_b)
                ch_b.start();
            else if (tmp & sw_rst_b)
                ch_b.reset();
            else
                ch_b.stop();
            last_state_b = tmp;
        }

        if ((tmp = (sw & (sw_rnd_a | sw_pnd_a))) != last_mode_a)
        {
            if (tmp & sw_rnd_a)
                ch_a.set_mode(RANDOM);
            else if (tmp & sw_pnd_a)
                ch_a.set_mode(PENDULUM);
            else
                ch_a.set_mode(NORMAL);
            last_mode_a = tmp;
        }

        if ((tmp = (sw & (sw_rnd_b | sw_pnd_b))) != last_mode_b)
        {
            if (tmp & sw_rnd_b)
                ch_b.set_mode(RANDOM);
            else if (tmp & sw_pnd_b)
                ch_b.set_mode(PENDULUM);
            else
                ch_b.set_mode(NORMAL);
            last_mode_b = tmp;
        }

        if ((tmp = (sw & (sw_inc | sw_dec))) != last_jog)
        {
            if (tmp)
            {
                if (sw & sw_side)
                    ch_b.jog(tmp & sw_dec);
                else
                    ch_a.jog(tmp & sw_dec);
            }
            last_jog = tmp;
        }

        if ((tmp = (sw & sw_trg)) != last_trg)
        {
            if (sw & sw_side)
                ch_b.fire(tmp);
            else
                ch_a.fire(tmp);
            last_trg = tmp;
        }

        last_sw = sw;
    }

    // update leds and read levels

    uint8_t step_a = ch_a.step(), step_b = ch_b.step();

    for (uint8_t ss = 0; ss < n_subseqs; ++ss)
    {
#if USE_UART
        printf("ss%d\n", ss);
#endif
        uint8_t led_cmd[2] = { 0, 0 };

        led_cmd[1] = ((ss == step_a >> 2) ? 1 << (step_a & 0x03) : 0)
                   | ((ss == step_b >> 2) ? 1 << (4 + (step_b & 0x03)) : 0)
                   ;

        twi::write(twi_addr[ss], led_cmd, sizeof(led_cmd));
        twi::wait_idle();
        delay_us(100);

        uint8_t offset = ss << 2;

        for (uint8_t i = 0; i < 8; ++i)
        {
            uint8_t read_cmd[2] = { 1, i };
            uint16_t x = 0;

            twi::write_read(twi_addr[ss], read_cmd, sizeof(read_cmd), reinterpret_cast<uint8_t*>(&x), sizeof(x));
            twi::wait_idle();
            delay_us(100);

            if (i < 4)
                ch_a.set_level(offset + i, x);
            else
                ch_b.set_level(offset + i - 4, x);
        }
    }

    // update start and finish steps

    uint8_t n_steps = n_subseqs << 2;
    uint16_t scale = 1024 / n_steps;

    ch_a.set_start((1023 - adc::read<adc_start_a>()) / scale);
    ch_a.set_finish((1023 - adc::read<adc_finish_a>()) / scale);
    ch_b.set_start((1023 - adc::read<adc_start_b>()) / scale);
    ch_b.set_finish((1023 - adc::read<adc_finish_b>()) / scale);

    delay_us(500);
}

