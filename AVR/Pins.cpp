#include <iostream>
#include <bitset>

typedef unsigned char unit8_t;

static unit8_t DDRB = 0, PORTB = 0, PINB = 0;
static unit8_t DDRC = 0, PORTC = 0, PINC = 0;
static unit8_t DDRD = 0, PORTD = 0, PIND = 0;

#include "Pins.h"

typedef Pd0 step_pin;
typedef Pd1 dir_pin;

void show()
{
	std::cout << std::bitset<8>(DDRB) << " " << std::bitset<8>(PORTB) << " " << std::bitset<8>(PINB) << " "
			  << std::bitset<8>(DDRC) << " " << std::bitset<8>(PORTC) << " " << std::bitset<8>(PINC) << " "
			  << std::bitset<8>(DDRD) << " " << std::bitset<8>(PORTD) << " " << std::bitset<8>(PIND) << std::endl;
}

int main()
{
	show();
	output_mode<dir_pin>();		show();
	output_mode<step_pin>();	show();
	write<dir_pin>(true);		show();
	write<step_pin>(true);		show();
	toggle<step_pin>();			show();
	toggle<step_pin>();			show();
	write<dir_pin>(false);		show();
	toggle<step_pin>();			show();
}

