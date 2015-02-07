#ifndef MOTOR_SHIELD_H
#define MOTOR_SHIELD_H 1

class MotorShieldV2
{
public:
	MotorShieldV2(uint8_t addr = 0x60);
	void reset(void);
	void setPWMFreq(float freq);
	void setPWM(uint8_t num, uint16_t on, uint16_t off);

private:
	uint8_t read8(uint8_t addr);
	void write8(uint8_t addr, uint8_t d);

	uint8_t m_i2caddr;
};

enum HBridgeEnum { HBridge1, HBridge2, HBridge3, HBridge4 };

enum HBridgeState { Off, Forward, Reverse };

class HBridge
{
public:
	HBridge(MotorShieldV2 *shield, HBridgeEnum e);
	void setPWM(uint16_t x);
	void set(HBridgeState s);

private:
	void set(uint8_t pin, bool x);

	MotorShieldV2	*m_shield;
	uint8_t			m_a, m_b, m_pwm;
};

enum StepperEnum { Stepper1, Stepper2 };

class Stepper
{
public:
	Stepper(MotorShieldV2 *shield, StepperEnum e);
	void step();

private:
	HBridge		m_a, m_b;
	uint8_t		m_i;
};

#endif // MOTOR_SHIELD

