#include <AVR/Main.h>
#include <AVR/Pins.h>
#include <AVR/Delay.h>
#include <AVR/LCD1602A.h>

typedef pin_t<PB, 3> LED;
typedef pin_t<PB, 0> CLOCK;
typedef pin_t<PB, 1> LATCH;
typedef pin_t<PB, 2> DATA;

typedef lcd1602a_t<DATA, CLOCK, LATCH> lcd;

static const char *fortunes[] =
    { "You can never be sure how many beers you had last night."
    , "We have reason to be afraid. This is a terrible place."
    , "It is annoying to be honest to no purpose."
    , "Man belongs wherever he wants to go."
    , "Inspiration without perspiration is usually sterile."
    , "Beauty may be skin deep, but ugly goes clear to the bone."
    , "You're either part of the solution or part of the problem."
    };

static const uint16_t n_fortunes = sizeof(fortunes) / sizeof(*fortunes);

void setup()
{
    digital_out<LED>();
    lcd::setup();
}

void loop()
{
    static uint16_t i = 0;

    toggle<LED>();

    lcd::clear();
    lcd::cursor(i & 1, i & 2);
    lcd::set_pos(i & 1, i & 2);
    delay_ms(500);
    lcd::write(fortunes[i]);
    if (++i >= n_fortunes)
        i = 0;
    delay_ms(1000);
}

