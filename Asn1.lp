/* Objective function */
max: 385 L1 +385 L2 + 385 L3 + 330 M1 + 330 M2 + 330 M3 + 275 S1 +275 S2 +275 S3;

/* Constraints */
Plant1: +L1+M1+S1 <= 750;
Plant2: +L2+M2+S2 <= 900;
Plant3: +L3+M3+S3 <= 450;

Plant1: +20L1+15M1+12S1 <= 13000;
Plant2: +20L2+15M2+12S2 <= 12000;
Plant3: +20L3+15M3+12S3 <= 5000;

    +L1+L2+l3 <= 900;
    +M1+M2+M3 <= 1200;
    +S1+S2+S3 <= 750;
