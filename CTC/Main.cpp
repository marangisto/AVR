#if 0

#include <AVR/Delay.h>
#include <AVR/Timer.h>
#include <Arduino//DFR0009.h>

typedef timer_t<1> W;


void setup()
{
    W::setup<ctc_mode, top_ocra>();
    W::clock_select<1024>();
    W::output_pin<channel_a>::setup();
    W::compare_output_mode<channel_a, toggle_on_compare_match>();
}


void loop()
{
    W::output_compare_register<channel_a>() = 77;

    delay_ms(2000);
}

int main()
{
	setup();
	for (;;)
		loop();
}

#else

#include <avr/io.h>
#include <util/delay.h>

struct CTC1
{
    static void setup()
    {
        // CTC mode with TOP-OCR1A
 
        TCCR1A = 0;
        TCCR1B = _BV(WGM12); 

        // toggle channel A on compare match
    
        TCCR1A = (TCCR1A & ~(_BV(COM1A1) | _BV(COM1A0))) | _BV(COM1A0);
  
        // set channel A bound pin PB1 to output mode

#if defined(__AVR_ATmega32U4__)
        DDRB |= _BV(5);
#else
        DDRB |= _BV(1);
#endif
        DDRB |= _BV(0);
    }

    static void set_freq(float f)
    {
        static const float f1 = min_freq(1), f8 = min_freq(8), f64 = min_freq(64), f256 = min_freq(256);

        uint16_t n;

        if (f >= f1)        n = 1;
        else if (f >= f8)   n = 8;
        else if (f >= f64)  n = 64;
        else if (f >= f256) n = 256;
        else                n = 1024;

        prescale(n);
  
        OCR1A = static_cast<uint16_t>(round(F_CPU / (2 * n * f) - 1));
    }

    static void set_ocr(uint16_t x)
    {
        while (TCNT1 > x)
            ;
        OCR1A = x;
    }

    static void prescale(uint16_t n)
    {
        uint8_t bits = 0;

        switch (n)
        {
            case    1:  bits = _BV(CS10);               break;
            case    8:  bits = _BV(CS11);               break;
            case   64:  bits = _BV(CS11) | _BV(CS10);   break;
            case  256:  bits = _BV(CS12);               break;
            case 1024:  bits = _BV(CS12) | _BV(CS10);   break;
            default:    bits = 0;
        }

        TCCR1B = (TCCR1B & ~(_BV(CS12) | _BV(CS11) | _BV(CS10))) | bits;
    }

    static inline float min_freq(uint16_t n)
    {
        return ceil(F_CPU / (2 * n * 65536));
    }
};

void setup()
{
    CTC1::setup();
    CTC1::prescale(1);
}

void loop()
{
    static bool odd = false;

    PORTB ^= _BV(0);
    CTC1::set_ocr(odd ? 1000 : 3000);
    odd = !odd;
    _delay_ms(1);
}

int main()
{
	setup();
	for (;;)
		loop();
}

#endif

