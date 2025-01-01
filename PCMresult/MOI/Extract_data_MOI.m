
filename = 'Guangdong_2016_2019MOI.xlsx';


data = xlsread(filename);


MOI= data(:, 1);

disp(MOI);



T = data(:, 2);


disp(T);


P = data(:, 3);
disp(P);

U = data(:, 4);
disp(U);

Ff = data(:, 5);
disp(Ff);
Tn = data(:, 6);
disp(Tn);
Tx = data(:, 7);
disp(Tx);
VV = data(:, 8);

disp(VV);
Td = data(:, 9);

disp(Td);
RRR = data(:, 10);

disp(RRR);






