#include <AVR/TWI.h>
#include <AVR/Main.h>
#include <AVR/Delay.h>
#include <Arduino//DFR0009.h>

typedef D13 LED;
typedef dfr0009_t lcd;
typedef buttons_t btns;

void setup()
{
	LED::setup();
    lcd::setup();
    btns::setup();
    twi_t::setup();
    sei();
}

void loop()
{
    static uint8_t x = 0;
    static uint8_t i = 0;
    static uint8_t err = 0;

    bool update = false;

    switch (btns::read())
    {
    case btn_up:
        ++x;
        update = true;
        break;
    case btn_down:
        --x;
        update = true;
        break;
    case btn_select:
        {
            uint8_t buf[256] = { 0x00, 0x0 };

            //err = twi_t::write(0x60, buf, sizeof(buf));
            err = twi_t::read(0x60, buf, x);
        }
        update = true;
        break;
    default: ;
    }

    if (update)
    {
        lcd::set_pos(0, 0);
        lcd::write(x, 16);
        lcd::write("  ");
        lcd::set_pos(1, 0);
        lcd::write(err, 16);
        lcd::write("  ");
    }

    if (++i == 0)
	    LED::toggle();

	delay_ms(1);
}


/*

void loop()
{
//	static const uint8_t sla = 0x68;		// DS1307 address (shifted?)
	static const uint8_t sla = 0xA0;		// 24C32 address (shifted?)
	static uint8_t i = 0, j = 0;

	if (i < 250)
	{
		uint8_t buf[] = { 0, i,  i, i + 1, i + 2, i + 3 };

		twi_t::write(0xA0, buf, sizeof(buf));
		delay_ms(10);
		twi_t::wait_idle();
		i += 4;
	}
	else
	{
		uint8_t adr[2] = { 0, j++ }, xs[8];

		twi_t::write_read(sla, adr, sizeof(adr), xs, sizeof(xs));
		twi_t::wait_idle();
		for (uint8_t k = 0; k < sizeof(xs); ++k)
		{
			seg7::write(xs[k]);
			delay_ms(100);
		}
		delay_ms(250);
	}
}

*/

