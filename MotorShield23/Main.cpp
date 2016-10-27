#include <AVR/Main.h>
#include <AVR/Delay.h>
#include <Arduino/Pins.h>
#include <Arduino/MotorShield23.h>

typedef D13 LED;
typedef motor_shield_23_t<> ms23;

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
    static bridge_t bridges[] = { ms23::bridge<0>(), ms23::bridge<1>(), ms23::bridge<2>(), ms23::bridge<3>() };
    static bridge_t bridge(bridges[0]);
    static uint8_t bix = 0;
    static int state = 0;
    static int speed = 0;
    static int step = 64;
    static bool rev = false;

    switch (state)
    {
    case 0:                 // initial state
        speed = 0;
        bridge.duty_cycle(speed);
        bridge.command(rev ? ms_reverse : ms_forward);
        state = 1;
        break;
    case 1:                 // accelerating
        if (speed < 4096)
        {
            speed += step;
            bridge.duty_cycle(speed);
        }
        else
            state = 2;
        break;
    case 2:                 // decelerating
        if (speed > 0)
        {
            speed -= step;
            bridge.duty_cycle(speed);
        }
        else
        {
            bridge.command(ms_stop);
            if (rev)
                bridge = bridges[++bix % (sizeof(bridges) / sizeof(*bridges))];
            rev = !rev;
            state = 0;
        }
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

