#include "../AVR/Pins.h"
#include "../AVR/Delay.h"
#include "../AVR/Timer1.h"
#include "../AVR/LCD1602A.h"
#include "Buttons.h"
#include "Editor.h"
#include "Accel.h"

typedef pin_t<PC, 3> CLOCK;
typedef pin_t<PC, 4> LATCH;
typedef pin_t<PC, 5> DATA;

typedef lcd1602a_t<DATA, CLOCK, LATCH> lcd;

typedef analog_input_t<2> A2;

typedef buttons_t<A2> btns;

typedef pin_t<PB, 1> DIR;
typedef pin_t<PB, 2> STEP;
typedef pin_t<PD, 4> RESET;
typedef pin_t<PD, 5> MS3;
typedef pin_t<PD, 6> MS2;
typedef pin_t<PD, 7> MS1;
typedef pin_t<PB, 0> ENABLE;

typedef a4988_t<DIR, STEP, RESET, MS1, MS2, MS3, ENABLE> a4988;

typedef accel_t<a4988> accel;

void setup()
{
	lcd::setup();
	btns::setup();
	accel::setup();
}

void loop()
{
	static item_t<bool> d("dir", false);
	static item_t<uint16_t> c("tmax", 16000);
	static item_t<micro_step_t::e> ms("u-step", micro_step_t::full_step);
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
			for (uint8_t j = 0; j < 4; ++j)
			{
				accel::run(d.value(), 200, c.value(), ms.value());
				d.incr();
			}
			break;
		case 5:
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


int main()
{
	setup();
	for (;;)
		loop();
}

