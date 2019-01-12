#include <AVR/TWI.h>
#include <AVR/Main.h>
#include <AVR/Delay.h>
#include <Arduino//DFR0009.h>

typedef D13 LED;
typedef dfr0009_t lcd;
typedef buttons_t btns;
typedef twi_master_t<0> twi;

void setup()
{
    LED::setup();
    lcd::setup();
    btns::setup();
    twi::setup();
    sei();

    for (uint8_t a = 0; a < 128; ++a)
    {
        uint8_t buf[1];

        if ((a & 0x78) == 0 || (a & 0x78) == 0x78)
            continue;   // reserved address
        if (twi::write(a, buf, 0) == 0)
        {
            lcd::clear();
            lcd::write("TWI DEVICE: 0x");
            lcd::write(a, 16);
            delay_ms(1000);
        }
    }

    lcd::clear();
    lcd::write("READY");
    delay_ms(1000);
    lcd::clear();
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

            twi::write(0x60, buf, 2);
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
//    static const uint8_t sla = 0x68;        // DS1307 address (shifted?)
    static const uint8_t sla = 0xA0;        // 24C32 address (shifted?)
    static uint8_t i = 0, j = 0;

    if (i < 250)
    {
        uint8_t buf[] = { 0, i,  i, i + 1, i + 2, i + 3 };

        twi::write(0xA0, buf, sizeof(buf));
        delay_ms(10);
        twi::wait_idle();
        i += 4;
    }
    else
    {
        uint8_t adr[2] = { 0, j++ }, xs[8];

        twi::write_read(sla, adr, sizeof(adr), xs, sizeof(xs));
        twi::wait_idle();
        for (uint8_t k = 0; k < sizeof(xs); ++k)
        {
            seg7::write(xs[k]);
            delay_ms(100);
        }
        delay_ms(250);
    }
}

*/

