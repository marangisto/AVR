#include <Wire.h>
#include <Arduino.h>
#include "MotorShield.h"

#define PCA9685_MODE1 0x0
#define PCA9685_PRESCALE 0xFE

MotorShieldV2::MotorShieldV2(uint8_t addr): m_i2caddr(addr)
{
	Wire.begin();
	write8(PCA9685_MODE1, 0x0);
	setPWMFreq(1600);

	for (uint8_t i=0; i<16; i++)
		setPWM(i, 0, 0);
}

void MotorShieldV2::setPWMFreq(float freq)
{
	float prescaleval = 25000000;
	prescaleval /= 4096;
	prescaleval /= freq;
	prescaleval -= 1;
	uint8_t prescale = floor(prescaleval + 0.5);
	uint8_t oldmode = read8(PCA9685_MODE1);
	uint8_t newmode = (oldmode&0x7F) | 0x10;	// sleep

	write8(PCA9685_MODE1, newmode);				// go to sleep
	write8(PCA9685_PRESCALE, prescale);			// set the prescaler
	write8(PCA9685_MODE1, oldmode);
	delay(5);
	write8(PCA9685_MODE1, oldmode | 0xa1);		//  This sets the MODE1 register to turn on auto increment
}

void MotorShieldV2::setPWM(uint8_t num, uint16_t on, uint16_t off)
{
	Wire.beginTransmission(m_i2caddr);
	Wire.write(0x6+4*num);
	Wire.write(on);
	Wire.write(on>>8);
	Wire.write(off);
	Wire.write(off>>8);
	Wire.endTransmission();
}

uint8_t MotorShieldV2::read8(uint8_t addr)
{
	Wire.beginTransmission(m_i2caddr);
	Wire.write(addr);
	Wire.endTransmission();
	Wire.requestFrom(static_cast<uint8_t>(m_i2caddr), static_cast<uint8_t>(1));
	return Wire.read();
}

void MotorShieldV2::write8(uint8_t addr, uint8_t d)
{
	Wire.beginTransmission(m_i2caddr);
	Wire.write(addr);
	Wire.write(d);
	Wire.endTransmission();
}

HBridge::HBridge(MotorShieldV2 *shield, HBridgeEnum e): m_shield(shield)
{
	switch (e)
	{
		case HBridge1: m_a = 10; m_b = 9;  m_pwm = 8; break;
		case HBridge2: m_a = 12; m_b = 11; m_pwm = 13; break;
		case HBridge3: m_a = 4;  m_b = 3;  m_pwm = 2; break;
		case HBridge4: m_a = 6;  m_b = 5;  m_pwm = 7; break;
	}

	set(Off);
	setPWM(4096);
}

void HBridge::setPWM(uint16_t x)
{
	if (x > 4095)
		m_shield->setPWM(m_pwm, 4096, 0);
	else
		m_shield->setPWM(m_pwm, 0, x);
}

void HBridge::set(HBridgeState s)
{
	switch (s)
	{
		case Off: set(m_a, false); set(m_b, false); break;
		case Forward: set(m_b, false); set(m_a, true); break;
		case Reverse: set(m_a, false); set(m_b, true); break;
	}
}

void HBridge::set(uint8_t pin, bool x)
{
	m_shield->setPWM(pin, x ? 4096 : 0, 0);
}

Stepper::Stepper(MotorShieldV2 *shield, StepperEnum e)
	: m_a(shield, e == Stepper1 ? HBridge1 : HBridge3)
	, m_b(shield, e == Stepper1 ? HBridge2 : HBridge4)
	, m_i(0)
{
}

void Stepper::setPWM(uint16_t x)
{
	m_a.setPWM(x);
	m_b.setPWM(x);
}

void Stepper::step()
{
	switch (m_i)
	{
		case 0: m_b.set(Off); m_a.set(Forward); ++m_i; break;
		case 1: m_a.set(Off); m_b.set(Forward); ++m_i; break;
		case 2: m_b.set(Off); m_a.set(Reverse); ++m_i; break;
		case 3: m_a.set(Off); m_b.set(Reverse); m_i = 0; break;
	}
}

