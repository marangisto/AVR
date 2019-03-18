#define NO_TIMER_VECTORS 1
#define USE_UART 0
#if defined(USE_UART)
#include <AVR/UART.h>
#endif
#include <AVR/TWI.h>
#include <AVR/ADC.h>
#include <AVR/Main.h>
#include <AVR/Delay.h>
#include <AVR/Timer.h>
#include <AVR/Pins.h>

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

typedef twi_master_t<0> twi;

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
#if defined(USE_UART)
            printf("found sub-sequence at 0x%02x\n", a);
#endif
        }
    }
}

typedef timer_t<3> pwm_a;     // channel-a (cv1, cv2)
typedef timer_t<1> pwm_b;     // channel-b (cv1, cv2)
typedef timer_t<2> aux;

enum action_t { no_action, play_step, play_no_advance };

static const uint16_t aux_prescale = 64;
static volatile uint16_t aux_count = 100;
static volatile bool auto_step = true;
static volatile action_t action = no_action;

ISR(TIMER2_OVF_vect)
{
    static uint16_t i = 0;

    if (auto_step && i++ >= aux_count)
    {
        action = play_step;
        i = 0;
    }
}

enum state_t { STOPPED, RUNNING };

template<class CLK, class RST, class TRIG_A, class TRIG_B, class PWM>
class channel_t
{
public:
    void init(uint8_t nsteps)
    {
        m_nsteps = nsteps;
        m_state = STOPPED;
        m_step = 0;
        m_start = 0;
        m_finish = nsteps - 1;
    }

    inline void start() { m_state = RUNNING; }
    inline void stop() { m_state = STOPPED; }
    inline void reset() { m_step = m_finish; }  // next clock will be start step

    inline state_t state() const { return m_state; }
    inline uint8_t step() const { return m_step; }

    inline void set_level(uint8_t i, uint16_t x)
    {
        if (i < sizeof(m_level) / sizeof(*m_level))
            m_level[i] = x;
    }

    inline void advance()
    {
        if (m_step >= m_finish)
            m_step = m_start;
        else
            ++m_step;
    }

    inline void isr()
    {
        static bool last_clk;

        if (!RST::read())
            reset();

        bool clk = !CLK::read();

        if (clk != last_clk)
        {
            if (clk && m_state == RUNNING)
            {
                advance();
                PWM::template output_compare_register<channel_a>() = m_level[m_step] >> 1;
                PWM::template output_compare_register<channel_b>() = m_level[m_step] >> 1;
                TRIG_A::set();
            }
            else
                TRIG_A::clear();
            last_clk = clk;
        }
    }

private:
    volatile uint8_t    m_nsteps;
    volatile state_t    m_state;
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
static const uint16_t sw_up_a = (1 << 0);    // a mode swith up
static const uint16_t sw_dn_a = (1 << 4);    // a mode swith down

static const uint16_t sw_run_b = (1 << 3);   // run state b
static const uint16_t sw_rst_b = (1 << 7);   // reset b
static const uint16_t sw_up_b = (1 << 6);    // b mode switch up
static const uint16_t sw_dn_b = (1 << 10);   // b mode switch down

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

/*
static void get_subseq_slot(bool side, uint8_t step, uint8_t& subseq, uint8_t& slot)
{
    uint8_t n_steps = n_subseqs << 2;   // 4-slots per sub-sequence module

    if (step < n_steps)
    {
        subseq = step >> 2;
        slot = (step & 0x03) + (side ? 4 : 0);
    }
    else
        subseq = slot = 0;                // this is an illegal step value...
}
*/
void setup()
{
#if defined(USE_UART)
    UART::setup<115200>();
#endif
    adc::setup<128>();

    clk_a::setup();
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
    pwm_a::compare_output_mode<channel_a, clear_on_compare_match>();
    pwm_a::compare_output_mode<channel_b, clear_on_compare_match>();

    pwm_b::setup<fast_pwm, top_0x1ff>();
    pwm_b::clock_select<1>();
    pwm_b::output_pin<channel_a>::setup();
    pwm_b::output_pin<channel_b>::setup();
    pwm_b::compare_output_mode<channel_a, clear_on_compare_match>();
    pwm_b::compare_output_mode<channel_b, clear_on_compare_match>();

    aux::setup<normal_mode>();
    aux::clock_select<aux_prescale>();
    aux::enable();

    twi::setup();
    sei();

#if defined(USE_UART)
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
    //static uint8_t ia = 0, ib = 0;
    static uint16_t last_state_a = 0;
    //static uint16_t last_mode_a = 0;
    static uint16_t last_state_b = 0;
    //static uint16_t last_mode_b = 0;
    uint16_t sw = scan_switches(), tmp = 0;

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

    /*

    auto_step = sw & sw_run_a;

    if (action != no_action)
    {
        uint8_t last_ia = ia;

        if (action == play_step)
        {
            ia = (ia + 1) & 0x0f;
            ib = (ib + 1) & 0x0f;
        }

        uint8_t led_cmd[2] = { 0, 0 };
        uint8_t subseq, slot;

        get_subseq_slot(false, last_ia, subseq, slot);
        led_state[subseq] &= ~(1 << slot);
        led_cmd[1] = led_state[subseq];
        twi::write(twi_addr[subseq], led_cmd, sizeof(led_cmd));
        twi::wait_idle();
        delay_us(100);

        get_subseq_slot(false, ia, subseq, slot);
        led_state[subseq] |= (1 << slot);
        led_cmd[1] = led_state[subseq];
        twi::write(twi_addr[subseq], led_cmd, sizeof(led_cmd));
        twi::wait_idle();
        delay_us(100);

        uint8_t read_cmd[2] = { 1, slot };
        uint16_t value = 0;

        twi::write_read(twi_addr[subseq], read_cmd, sizeof(read_cmd), reinterpret_cast<uint8_t*>(&value), sizeof(value));
        twi::wait_idle();
        bool sw_1 = (value & (1 << 13)) != 0;
        bool sw_2 = (value & (1 << 14)) != 0;
        //printf("%d %d %s\n", i, value, sw_a ? "a" : (sw_b ? "b" : " "));
        if (sw_1)
        {
            pwm_a::output_compare_register<channel_a>() = (value >> 2);
            trig_a1::set();
            delay_us(100);
            trig_a1::clear();
        }
        else if (sw_2)
        {
            pwm_a::output_compare_register<channel_b>() = (value >> 2);
            trig_a2::set();
            delay_us(100);
            trig_a2::clear();
        }
        action = no_action;
    }
    */

    delay_us(500);
}

