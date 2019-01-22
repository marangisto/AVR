#define NO_TIMER_VECTORS 1
//#include <AVR/UART.h>
#include <AVR/TWI.h>
#include <AVR/ADC.h>
#include <AVR/Main.h>
#include <AVR/Delay.h>
#include <AVR/Timer.h>
#include <AVR/Pins.h>

/*
typedef output_t<PB, 1> cv_1a;
typedef output_t<PB, 2> cv_1b;
typedef output_t<PD, 2> cv_2a;
typedef output_t<PD, 0> cv_2b;
*/

typedef output_t<PB, 0> trig_1a;
typedef output_t<PB, 3> trig_1b;
typedef output_t<PB, 4> trig_2a;
typedef output_t<PB, 5> trig_2b;

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
            //printf("found sub-sequence at 0x%02x\n", a);
        }
    }
}

typedef timer_t<1> pwm;
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


/*
static uint8_t bit(uint16_t x)
{
    for (uint8_t i = 0; i < 16; ++i)
        if (x & (1 << i))
            return i + 1;
    return 0xf0;
}
*/

void setup()
{
    //UART::setup<115200>();
    adc::setup<128>();

    trig_1a::setup();
    trig_1b::setup();
    trig_2a::setup();
    trig_2b::setup();

    /*
    cv_1a::setup();
    cv_1b::setup();
    cv_2a::setup();
    cv_2b::setup();
    */

    sense0::setup();
    sense1::setup();
    sense2::setup();

    scan0::setup();
    scan1::setup();
    scan2::setup();
    scan3::setup();

    pwm::setup<fast_pwm, top_0x1ff>();
    pwm::clock_select<1>();
    pwm::output_pin<channel_a>::setup();
    pwm::output_pin<channel_b>::setup();
    pwm::compare_output_mode<channel_a, clear_on_compare_match>();
    pwm::compare_output_mode<channel_b, clear_on_compare_match>();

    aux::setup<normal_mode>();
    aux::clock_select<aux_prescale>();
    aux::enable();

    twi::setup();
    sei();

    //printf("Marangisto Sequence 0.1\n");

    attach_subseqs();
}

void loop()
{
    static uint8_t i = 0;

    uint16_t sw = scan_switches();

    auto_step = sw & sw_run_a;

    if (action != no_action)
    {
        uint8_t last_i = i;

        if (action == play_step)
            i = (i + 1) & 0x07;

        uint8_t led_cmd[2] = { 0, 0 };
        uint8_t subseq, slot;

        get_subseq_slot(false, last_i, subseq, slot);
        led_state[subseq] &= ~(1 << slot);
        led_cmd[1] = led_state[subseq];
        twi::write(twi_addr[subseq], led_cmd, sizeof(led_cmd));
        twi::wait_idle();
        delay_us(100);

        get_subseq_slot(false, i, subseq, slot);
        led_state[subseq] |= (1 << slot);
        led_cmd[1] = led_state[subseq];
        twi::write(twi_addr[subseq], led_cmd, sizeof(led_cmd));
        twi::wait_idle();
        delay_us(100);

        uint8_t read_cmd[2] = { 1, slot };
        uint16_t value = 0;

        twi::write_read(twi_addr[subseq], read_cmd, sizeof(read_cmd), reinterpret_cast<uint8_t*>(&value), sizeof(value));
        twi::wait_idle();
        bool sw_a = (value & (1 << 13)) != 0;
        bool sw_b = (value & (1 << 14)) != 0;
        //printf("%d %d %s\n", i, value, sw_a ? "a" : (sw_b ? "b" : " "));
        if (sw_a )
        {
            pwm::output_compare_register<channel_a>() = 0x1ff / (value >> 2);   // inverted output
            trig_1a::set();
            delay_us(100);
            trig_1a::clear();
        }
        else if (sw_b)
        {
            pwm::output_compare_register<channel_b>() = 0x1ff - (value >> 2);   // inverted output
            trig_1b::set();
            delay_us(100);
            trig_1b::clear();
        }
        action = no_action;
    }

    delay_us(500);
}

