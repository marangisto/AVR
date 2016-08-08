#include <AVR/TWI.h>
#include <AVR/Main.h>
#include <AVR/Delay.h>
#include <Arduino/DFR0009.h>
#include <Arduino/MotorShield23.h>

typedef D13 LED;
typedef dfr0009_t lcd;
typedef buttons_t btns;
typedef motor_shield_23_t<> ms23;
static const int bridge = 0;

void twi_error(uint8_t err, const char *file, uint16_t line)
{
    lcd::clear();
    lcd::set_pos(0, 0);
    lcd::write("TWI ERROR: 0x");
    lcd::write(err, 16);
    lcd::set_pos(1, 0);
    lcd::write(file);
    lcd::write(", ");
    lcd::write(line);

    while (true) ;  // stay here forever
}

void setup()
{
	LED::setup();
    lcd::setup();
    btns::setup();
    ms23::setup<0x1e>();  // keep last as it enables interrupts
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
    case btn_left:
        ms23::command<bridge>(ms23::reverse);
        break;
    case btn_right:
        ms23::command<bridge>(ms23::forward);
        break;
    case btn_select:
        ms23::command<bridge>(ms23::stop);
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

        ms23::duty_cycle<bridge>(x << 4);
    }

    if (++i == 0)
	    LED::toggle();

	delay_ms(1);
}

