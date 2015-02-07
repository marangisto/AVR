#include <Wire.h>

#include "MotorShield.h"

static Stepper *gM0 = 0;
static Stepper *gM1 = 0;

void setup()
{
  Serial.begin(9600);
  MotorShieldV2 *pwmdrv = new MotorShieldV2(0x60);

  gM0 = new Stepper(pwmdrv, 0);
  gM1 = new Stepper(pwmdrv, 1);
}

void loop()
{
	gM0->step();
	gM1->step();
  
	delay(100);
}


