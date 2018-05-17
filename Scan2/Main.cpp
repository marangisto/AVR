#include <AVR/Main.h>
#include <AVR/Pins.h>
#include <AVR/Delay.h>
#include <AVR/LCD1602A/SN74HC595.h>
#include <AVR/Buttons.h>
#include "Editor.h"
#include "Accel.h"

typedef output_t<PC, 3> CLOCK;
typedef output_t<PC, 4> LATCH;
typedef output_t<PC, 5> DATA;

typedef tc1602_t<sn74hc595_tc1602_t<DATA, CLOCK, LATCH> > lcd;

typedef buttons_t<2> btns;

typedef output_t<PB, 1> DIR;
typedef output_t<PB, 2> STEP;
typedef output_t<PD, 4> RESET;
typedef output_t<PD, 5> MS3;
typedef output_t<PD, 6> MS2;
typedef output_t<PD, 7> MS1;
typedef output_t<PB, 0> ENABLE;

typedef a4988_t<DIR, STEP, RESET, MS1, MS2, MS3, ENABLE> a4988;

typedef input_t<PD, 2, enable_pullup> LIML;
typedef input_t<PD, 3, enable_pullup> LIMR;

typedef accel_t<a4988, LIML, LIMR> accel;

void setup()
{
    lcd::setup();
    btns::setup();
    LIML::setup();
    LIMR::setup();
    accel::setup();
}

void loop()
{
    static item_t<bool> d("dir", false);
    static item_t<uint16_t> c("tmax", 10000);
    static item_t<micro_step_t::e> ms("u-step", micro_step_t::quarter_step);
    static item_i *items[] = { &d, &c, &ms };
    static editor_t editor(items, sizeof(items) / sizeof(*items));

    static bool refresh = true;
    static char buf[64];

    uint8_t x = btns::read();

    switch (x & btns::mask)
    {
        case 1: 
            editor.next();
            refresh = true;
            break;
        case 2:
            editor.decr((x & btns::fast) != 0);
            refresh = true;
            break;
        case 3:
            editor.incr((x & btns::fast) != 0);
            refresh = true;
            break;
        case 4:
        {
            int16_t err = 0;
            uint16_t c = 10000;

            do
            {
                lcd::clear();
                lcd::set_pos(0, 0);
                lcd::write(c);
                accel::run(false, 1000, c, ms.value());
                accel::run(true, 1000, c, ms.value());
                err = accel::calibrate();
                lcd::set_pos(0, 8);
                lcd::write("adj = ");
                lcd::write(err);
                lcd::set_pos(1, 0);
                lcd::write("min step = ");
                lcd::write(accel::min_step());
                c -= 250;
            } while (abs(err) < 2);
        } break;
        case 5:
            lcd::set_pos(1, 0);
            lcd::write("adj = ");
            lcd::write(accel::calibrate());
            lcd::write("       ");
            break;
        default: ;
    }

    if (refresh)
    {
        lcd::clear();
        lcd::set_pos(0, 0);
        lcd::write(editor.name());
        lcd::set_pos(0, 8);
        lcd::write(editor.show(buf));
        refresh = false;
    }

    delay_ms(1);
}

