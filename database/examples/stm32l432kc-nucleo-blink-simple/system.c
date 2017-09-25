void SystemInit (void) {

    RCC -> CR |= RCC_CR_HSION; //Set HSI on (will use it as temporary source)
    while ((RCC -> CR & RCC_CR_HSIRDY) == 0); //Wait until its ready
    RCC -> CFGR = ((RCC->CFGR & ~RCC_CFGR_SW_Msk) | RCC_CFGR_SW_HSI); //Switch to HSI

    RCC -> CR &= ~RCC_CR_MSION_Msk; //Turn MSI off
    while ((RCC -> CR & RCC_CR_MSIRDY) != 0); //Wait till it's truly off
    RCC -> CR &= ~RCC_CR_MSIRANGE_Msk; //Clean MSIRANGE bits for setting MSI speed
    RCC -> CR |= (0b1011 << RCC_CR_MSIRANGE_Pos); //Set speed to 48 MHz
    RCC -> CR |= RCC_CR_MSIRGSEL; //Use MSIRANGE instead of MSISRANGE
    RCC -> APB1ENR1 |= RCC_APB1ENR1_PWREN; //Turn power management module on
    PWR -> CR1 |= PWR_CR1_DBP; //Turn BDCR register write protection off
    RCC -> BDCR |= (0b11 << RCC_BDCR_LSEDRV_Pos); //Set LSE drive power to maximum
    RCC -> BDCR |= RCC_BDCR_LSEON; //Turn on LSE
    while ((RCC->BDCR & RCC_BDCR_LSERDY) == 0); //Wait till LSE is ready. Up to 2 seconds
    RCC -> CR |= RCC_CR_MSIPLLEN; //Turn on MSI clock correction via LSE
    RCC -> CR |= RCC_CR_MSION; //Turn MSI on
    while ((RCC -> CR & RCC_CR_MSIRDY) == 0); //Wait till it's ready
    RCC -> CFGR |= ( 0b1000 << RCC_CFGR_HPRE_Pos); //Clock prescaler for AHB bus, by 4. It can take 36Mhz max.
    RCC -> CFGR = ((RCC->CFGR & ~RCC_CFGR_SW_Msk) | RCC_CFGR_SW_MSI); //Switch clock source to MSI
}
