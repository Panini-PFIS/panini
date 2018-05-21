alter table user_lamina add constraint check_lamina_in_range check (lamina > 0 and lamina <= 670);

insert into "user" values (1, 'camilo', 'camilo', 70.0, -120.0);
insert into "user" values (2, 'jhordy', 'camilo', 70.0, -120.0);
insert into "user" values (3, 'carlos', 'camilo', 70.0, -120.0);

insert into user_lamina values (1,1,32,8);
insert into user_lamina values (2,1,24,10);
insert into user_lamina values (3,1,456,4);
insert into user_lamina values (4,2,670,2);
insert into user_lamina values (5,2,124,4);
insert into user_lamina values (6,3,99,2);
insert into user_lamina values (7,3,325,14);

insert into intercambio values (1,1,2,32,124, current_date);
