package com.ibm.cics.cip.cbsa.galasa.tests.manager;

import java.util.Calendar;
import java.util.GregorianCalendar;
import java.util.Random;

import com.ibm.cics.cip.cbsa.galasa.tests.manager.internal.CbsaTerminalImpl;

public interface ICbsaCustomer {
    public String getCustomerNumber();
    public ICbsaAccount generateAccount(String accountType) throws CbsaException;
    public void discardAccounts() throws CbsaException;
    public void addToAccountslist(ICbsaAccount account);
    public void discardSelf(CbsaTerminalImpl terminal) throws CbsaException;

    public static String[] generateRandomCustomerData() {
        Random rand = new Random(System.currentTimeMillis());

		String[] titles     = {"Mr","Mrs","Miss","Ms","Dr","Professor","Lord","Drs","Sir","Lady"};
		String[] firstName  = {"James","Daniel","Brad","Andy","Thomas","Chris","Jon","Ruth","Stella","Frank"};
		String[] lastName   = {"Ford","Corner","Desk","Lamp","Key","Sailor","Sing","Troth","Raut","Fernando"};
        String[] addr1      = {"1 Main Street","2 Elm Street","3 Pine Street","4 Oak Street","5 Maple Street","6 Birch Street","7 Cedar Street","8 Walnut Street","9 Cherry Street","2 Chestnut Street"};
        String[] cities     = {"Paris", "London", "Berlin", "Madrid", "Rome", "Vienna", "Amsterdam", "Copenhagen", "Stockholm", "Prague"};

        String middleInitials = String.valueOf((char) (rand.nextInt(26) + 'A'));
        String dob = generateDateOfBirth();

        return new String[]{
            titles[rand.nextInt(titles.length)],
            firstName[rand.nextInt(firstName.length)],
            middleInitials,
            lastName[rand.nextInt(lastName.length)],
            addr1[rand.nextInt(lastName.length)],
            "Address Line 2",
            "Address Line 3",
            dob,
            cities[rand.nextInt(cities.length)]
        };
    }

    public static String generateDateOfBirth() {
        GregorianCalendar gc = new GregorianCalendar();

        int year = randBetween(1900, 2010);

        gc.set(Calendar.YEAR, year);

        int dayOfYear = randBetween(1, gc.getActualMaximum(Calendar.DAY_OF_YEAR));

        gc.set(Calendar.DAY_OF_YEAR, dayOfYear);

        String dob = String.format("%02d/%02d/%2d", gc.get(Calendar.DAY_OF_MONTH), gc.get(Calendar.MONTH) + 1, gc.get(Calendar.YEAR));
        return dob;
    }

    public static int randBetween(int start, int end) {
        return start + (int)Math.round(Math.random() * (end - start));
    }
}
