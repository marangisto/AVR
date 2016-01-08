#include <AVR/Pins.h>
#include <AVR/Delay.h>
#include <AVR/Timer.h>

typedef output_t<PB, 1> LED;
typedef timer_t<1> T;

void setup()
{
	LED::setup();
    T::setup();
    T::start<8>();
    T::pwma();
}

void loop()
{
    //  map ((\x -> x - 1) . round . (2**) . (/3)) [0..30] -- adjust final element to be one less than TOP
    static const int dcs[] = { 0,0,1,1,2,2,3,4,5,7,9,12,15,19,24,31,39,50,63,80,101,127,160,202,255,322,405,511,644,812,1022 };
    static const unsigned dcs_size = sizeof(dcs) / sizeof(*dcs);
    static unsigned i = 0;
    static bool dir = false;            // scan direction

    T::ocra() = dcs[i];

    if (i == 0 || (i + 1) == dcs_size)
        dir = !dir;
    
    i += dir ? 1 : -1;

	delay_ms(125);
}

int main()
{
	setup();
	for (;;)
		loop();
}

