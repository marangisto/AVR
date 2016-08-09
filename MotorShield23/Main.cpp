#include <AVR/TWI.h>
#include <AVR/Main.h>
#include <AVR/Delay.h>
#include <Arduino/Pins.h>
#include <Arduino/MotorShield23.h>

typedef D13 LED;
typedef motor_shield_23_t<> ms23;
static const int bridge = 0;

void twi_error(uint8_t err, const char *file, uint16_t line)
{
    while (true)
    {
        LED::toggle();
        delay_ms(100);
    }
}

void setup()
{
	LED::setup();
    ms23::setup<0x1e>();  // keep last as it enables interrupts
}

static void update_motor()
{
    static int state = 0;
    static int speed = 0;
    static int step = 64;
    static bool rev = false;

    switch (state)
    {
    case 0:                 // initial state
        speed = 0;
        ms23::duty_cycle<bridge>(speed);
        ms23::command<bridge>(rev ? ms23::reverse : ms23::forward);
        rev = !rev;
        state = 1;
        break;
    case 1:                 // accelerating
        if (speed < 4096)
        {
            speed += step;
            ms23::duty_cycle<bridge>(speed);
        }
        else
            state = 2;
        break;
    case 2:                 // decelerating
        if (speed > 0)
        {
            speed -= step;
            ms23::duty_cycle<bridge>(speed);
        }
        else
            state = 0;
        break;
    }
}

void loop()
{
    static uint8_t i = 0;

    if ((i & 0xf) == 0)
        update_motor();

    if (++i == 0)
	    LED::toggle();

	delay_ms(1);
}

