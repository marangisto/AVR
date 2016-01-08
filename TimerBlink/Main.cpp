#include <AVR/Pins.h>
#include <AVR/Timer.h>
#include <AVR/Delay.h>

#if defined(__AVR_ATmega32U4__)
typedef output_t<PC, 7> LED;			// leonardo
#else
typedef output_t<PB, 5> LED;			// uno
#endif

typedef timer_t<1> timer;

static void isr()
{
	LED::toggle();
}

void setup()
{
	LED::setup();

	timer::setup<normal_mode>();
	timer::clock_select<64>();
	timer::isr(isr);
	timer::enable();
	sei();
}

void loop()
{
	// do something else
	delay_ms(1000);
}

int main()
{
	setup();
	for (;;)
		loop();
}

