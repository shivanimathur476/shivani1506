/* Objective Function */
 
max: ;

/* Constraints */

y1 - y2 <= 1;
y2 - y3 <= 1;
y1 - y2 >= -1;
y2 - y3 >= -1;

xa1 + xb1 + xc1 + xd1 + xe1 + xf1 + xg1  + xh1 + xi1 + xj1 + xk1 <= 4;
xa1 + xb1 + xc1 + xd1 + xe1 + xf1 + xg1  + xh1 + xi1 + xj1 + xk1 >= 3;
xa2 + xb2 + xc2 + xd2 + xe2 + xf2 + xg2  + xh2 + xi2 + xj2 + xk2 <= 4;
xa2 + xb2 + xc2 + xd2 + xe2 + xf2 + xg2  + xh2 + xi2 + xj2 + xk2 >= 3;
xa3 + xb3 + xc3 + xd3 + xe3 + xf3 + xg3  + xh3 + xi3 + xj3 + xk3 <= 4;
xa3 + xb3 + xc3 + xd3 + xe3 + xf3 + xg3  + xh3 + xi3 + xj3 + xk3 >= 3;

13.4xa1 + 10.2xb1 + 8.6xc1 + 13.2xd1 + 18xe1 + 16.6xf1 + 15.4xg1  + 16.8xh1 + 12xi1 + 13.4xj1 + 14xk1 = y1;
13.4xa2 + 10.2xb2 + 8.6xc2 + 13.2xd2 + 18xe2 + 16.6xf2 + 15.4xg2  + 16.8xh2 + 12xi2 + 13.4xj2 + 14xk2 = y2;
13.4xa3 + 10.2xb3 + 8.6xc3 + 13.2xd3 + 18xe3 + 16.6xf3 + 15.4xg3  + 16.8xh3 + 12xi3 + 13.4xj3 + 14xk3 = y3;

xa1 + xa2 + xa3 = 1;
xb1 + xb2 + xb3 = 1;
xc1 + xc2 + xc3 = 1;
xd1 + xd2 + xd3 = 1;
xe1 + xe2 + xe3 = 1;
xf1 + xf2 + xf3 = 1;
xg1 + xg2 + xg3 = 1;
xh1 + xh2 + xh3 = 1;
xi1 + xi2 + xi3 = 1;
xj1 + xj2 + xj3 = 1;
xk1 + xk2 + xk3 = 1;



