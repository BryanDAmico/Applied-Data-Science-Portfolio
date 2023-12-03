IF NOT EXISTS (SELECT * FROM SYS.DATABASES WHERE NAME = 'winerydev')
	CREATE DATABASE winerydev
GO

USE winerydev
GO


----------
-- DOWN --
----------

DROP VIEW IF EXISTS v_bottles_in_stock_by_product
GO

DROP VIEW IF EXISTS v_bottles_sold_by_product
GO

DROP VIEW IF EXISTS v_bottles_sold_and_in_stock
GO

DROP VIEW IF EXISTS v_product_order_count_by_year
GO

DROP VIEW IF EXISTS v_top_selling_products
GO

DROP VIEW IF EXISTS v_vine_to_bottle
GO

DROP VIEW IF EXISTS v_product_aging
GO

DROP VIEW IF EXISTS v_sales_revenue
GO



IF EXISTS (SELECT * FROM INFORMATION_SCHEMA.TABLE_CONSTRAINTS
	WHERE CONSTRAINT_NAME = 'pk_payments_payment_id')
	ALTER TABLE payments DROP CONSTRAINT pk_payments_payment_id

IF EXISTS (SELECT * FROM INFORMATION_SCHEMA.TABLE_CONSTRAINTS
	WHERE CONSTRAINT_NAME = 'fk1_payments_payment_customer_id')
	ALTER TABLE payments DROP CONSTRAINT fk1_payments_payment_customer_id

IF EXISTS (SELECT * FROM INFORMATION_SCHEMA.TABLE_CONSTRAINTS
	WHERE CONSTRAINT_NAME = 'fk2_payments_payment_order_id')
	ALTER TABLE payments DROP CONSTRAINT fk2_payments_payment_order_id

IF EXISTS (SELECT * FROM INFORMATION_SCHEMA.TABLE_CONSTRAINTS
	WHERE CONSTRAINT_NAME = 'u1_payments_payment_ccard_num')
	ALTER TABLE payments DROP CONSTRAINT u1_payments_payment_ccard_num

DROP TABLE IF EXISTS payments
GO


IF EXISTS (SELECT * FROM INFORMATION_SCHEMA.TABLE_CONSTRAINTS
	WHERE CONSTRAINT_NAME = 'pk_orders_order_id')
	ALTER TABLE orders DROP CONSTRAINT pk_orders_order_id

IF EXISTS (SELECT * FROM INFORMATION_SCHEMA.TABLE_CONSTRAINTS
	WHERE CONSTRAINT_NAME = 'fk1_orders_order_bottle_id')
	ALTER TABLE orders DROP CONSTRAINT fk1_orders_order_bottle_id

IF EXISTS (SELECT * FROM INFORMATION_SCHEMA.TABLE_CONSTRAINTS
	WHERE CONSTRAINT_NAME = 'fk2_orders_order_customer_id')
	ALTER TABLE orders DROP CONSTRAINT fk2_orders_order_customer_id

IF EXISTS (SELECT * FROM INFORMATION_SCHEMA.TABLE_CONSTRAINTS
	WHERE CONSTRAINT_NAME = 'fk3_orders_order_shipper_id')
	ALTER TABLE orders DROP CONSTRAINT fk3_orders_order_shipper_id

DROP TABLE IF EXISTS orders
GO


IF EXISTS (SELECT * FROM INFORMATION_SCHEMA.TABLE_CONSTRAINTS
	WHERE CONSTRAINT_NAME = 'pk_shippers_shipper_id')
	ALTER TABLE shippers DROP CONSTRAINT pk_shippers_shipper_id

IF EXISTS (SELECT * FROM INFORMATION_SCHEMA.TABLE_CONSTRAINTS
	WHERE CONSTRAINT_NAME = 'u1_shippers_shipper_name')
	ALTER TABLE shippers DROP CONSTRAINT u1_shippers_shipper_name

IF EXISTS (SELECT * FROM INFORMATION_SCHEMA.TABLE_CONSTRAINTS
	WHERE CONSTRAINT_NAME = 'u2_shippers_shipper_phone_number')
	ALTER TABLE shippers DROP CONSTRAINT u2_shippers_shipper_phone_number

DROP TABLE IF EXISTS shippers
GO


IF EXISTS (SELECT * FROM INFORMATION_SCHEMA.TABLE_CONSTRAINTS
	WHERE CONSTRAINT_NAME = 'pk_customers_customer_id')
	ALTER TABLE customers DROP CONSTRAINT pk_customers_customer_id

IF EXISTS (SELECT * FROM INFORMATION_SCHEMA.TABLE_CONSTRAINTS
	WHERE CONSTRAINT_NAME = 'u1_customers_customer_email')
	ALTER TABLE customers DROP CONSTRAINT u1_customers_customer_email

DROP TABLE IF EXISTS customers
GO


IF EXISTS (SELECT * FROM INFORMATION_SCHEMA.TABLE_CONSTRAINTS
	WHERE CONSTRAINT_NAME = 'pk_bottles_bottle_id')
	ALTER TABLE bottles DROP CONSTRAINT pk_bottles_bottle_id

IF EXISTS (SELECT * FROM INFORMATION_SCHEMA.TABLE_CONSTRAINTS
	WHERE CONSTRAINT_NAME = 'u1_bottles_bottle_label_number')
	ALTER TABLE bottles DROP CONSTRAINT u1_bottles_bottle_label_number

IF EXISTS (SELECT * FROM INFORMATION_SCHEMA.TABLE_CONSTRAINTS
	WHERE CONSTRAINT_NAME = 'fk1_bottles_bottle_barrel_id')
	ALTER TABLE bottles DROP CONSTRAINT fk1_bottles_bottle_barrel_id

IF EXISTS (SELECT * FROM INFORMATION_SCHEMA.TABLE_CONSTRAINTS
	WHERE CONSTRAINT_NAME = 'chk1_bottles_bottle_sold')
	ALTER TABLE bottles DROP CONSTRAINT chk1_bottles_bottle_sold

IF EXISTS (SELECT * FROM INFORMATION_SCHEMA.TABLE_CONSTRAINTS
	WHERE CONSTRAINT_NAME = 'fk2_bottles_bottle_product_id')
	ALTER TABLE bottles DROP CONSTRAINT fk2_bottles_bottle_product_id

DROP TABLE IF EXISTS bottles
GO


IF EXISTS (SELECT * FROM INFORMATION_SCHEMA.TABLE_CONSTRAINTS
	WHERE CONSTRAINT_NAME = 'pk_products_product_id')
	ALTER TABLE products DROP CONSTRAINT pk_products_product_id

IF EXISTS (SELECT * FROM INFORMATION_SCHEMA.TABLE_CONSTRAINTS
	WHERE CONSTRAINT_NAME = 'u1_products_product_name')
	ALTER TABLE products DROP CONSTRAINT u1_products_product_name

DROP TABLE IF EXISTS products
GO


IF EXISTS (SELECT * FROM INFORMATION_SCHEMA.TABLE_CONSTRAINTS
	WHERE CONSTRAINT_NAME = 'pk_barrels_barrel_id')
	ALTER TABLE barrels DROP CONSTRAINT pk_barrels_barrel_id

IF EXISTS (SELECT * FROM INFORMATION_SCHEMA.TABLE_CONSTRAINTS
	WHERE CONSTRAINT_NAME = 'u1_barrels_barrel_number')
	ALTER TABLE barrels DROP CONSTRAINT u1_barrels_barrel_number

IF EXISTS (SELECT * FROM INFORMATION_SCHEMA.TABLE_CONSTRAINTS
	WHERE CONSTRAINT_NAME = 'chk1_barrels_barrel_start_date')
	ALTER TABLE barrels DROP CONSTRAINT chk1_barrels_barrel_start_date

IF EXISTS (SELECT * FROM INFORMATION_SCHEMA.TABLE_CONSTRAINTS
	WHERE CONSTRAINT_NAME = 'chk2_barrels_barrel_end_date')
	ALTER TABLE barrels DROP CONSTRAINT chk2_barrels_barrel_end_date

IF EXISTS (SELECT * FROM INFORMATION_SCHEMA.TABLE_CONSTRAINTS
	WHERE CONSTRAINT_NAME = 'fk1_barrels_barrel_fermenter_id')
	ALTER TABLE barrels DROP CONSTRAINT fk1_barrels_barrel_fermenter_id

	
IF EXISTS (SELECT * FROM INFORMATION_SCHEMA.TABLE_CONSTRAINTS
	WHERE CONSTRAINT_NAME = 'fk2_barrels_barrel_cellar_id')
	ALTER TABLE barrels DROP CONSTRAINT fk2_barrels_barrel_cellar_id

DROP TABLE IF EXISTS barrels
GO


IF EXISTS (SELECT * FROM INFORMATION_SCHEMA.TABLE_CONSTRAINTS
	WHERE CONSTRAINT_NAME = 'pk_cellars_cellar_id')
	ALTER TABLE cellars DROP CONSTRAINT pk_cellars_cellar_id

DROP TABLE IF EXISTS cellars
GO


IF EXISTS (SELECT * FROM INFORMATION_SCHEMA.TABLE_CONSTRAINTS
	WHERE CONSTRAINT_NAME = 'pk_vineyards_planting_harvest_id')
	ALTER TABLE vineyards_planting_harvests DROP CONSTRAINT pk_vineyards_planting_harvest_id

IF EXISTS (SELECT * FROM INFORMATION_SCHEMA.TABLE_CONSTRAINTS
	WHERE CONSTRAINT_NAME = 'fk1_vineyards_planting_harvests_vineyard_id')
	ALTER TABLE vineyards_planting_harvests DROP CONSTRAINT fk1_vineyards_planting_harvests_vineyard_id

IF EXISTS (SELECT * FROM INFORMATION_SCHEMA.TABLE_CONSTRAINTS
	WHERE CONSTRAINT_NAME = 'chk1_vineyards_planting_harvest_vineyard_section')
	ALTER TABLE vineyards_planting_harvests DROP CONSTRAINT chk1_vineyards_planting_harvest_vineyard_section

IF EXISTS (SELECT * FROM INFORMATION_SCHEMA.TABLE_CONSTRAINTS
	WHERE CONSTRAINT_NAME = 'chk2_vineyards_planting_harvests_vineyard_plant_date')
	ALTER TABLE vineyards_planting_harvests DROP CONSTRAINT chk2_vineyards_planting_harvests_vineyard_plant_date

	
IF EXISTS (SELECT * FROM INFORMATION_SCHEMA.TABLE_CONSTRAINTS
	WHERE CONSTRAINT_NAME = 'chk3_vineyards_planting_harvests_vineyard_harvest_date')
	ALTER TABLE vineyards_planting_harvests DROP CONSTRAINT chk3_vineyards_planting_harvests_vineyard_harvest_date

DROP TABLE IF EXISTS vineyards_planting_harvests
GO


IF EXISTS (SELECT * FROM INFORMATION_SCHEMA.TABLE_CONSTRAINTS
	WHERE CONSTRAINT_NAME = 'pk_fermenters_vineyards_fermenter_batch_id')
	ALTER TABLE fermenters_vineyards DROP CONSTRAINT pk_fermenters_vineyards_fermenter_batch_id

IF EXISTS (SELECT * FROM INFORMATION_SCHEMA.TABLE_CONSTRAINTS
	WHERE CONSTRAINT_NAME = 'fk1_fermenters_vineyards_fermenter_id')
	ALTER TABLE fermenters_vineyards DROP CONSTRAINT fk1_fermenters_vineyards_fermenter_id

	
IF EXISTS (SELECT * FROM INFORMATION_SCHEMA.TABLE_CONSTRAINTS
	WHERE CONSTRAINT_NAME = 'fk2_fermenters_vineyards_fermenter_vineyard_id')
	ALTER TABLE fermenters_vineyards DROP CONSTRAINT fk2_fermenters_vineyards_fermenter_vineyard_id

DROP TABLE IF EXISTS fermenters_vineyards
GO


IF EXISTS (SELECT * FROM INFORMATION_SCHEMA.TABLE_CONSTRAINTS
	WHERE CONSTRAINT_NAME = 'pk_vineyards_vineyard_id')
	ALTER TABLE vineyards DROP CONSTRAINT pk_vineyards_vineyard_id

	
IF EXISTS (SELECT * FROM INFORMATION_SCHEMA.TABLE_CONSTRAINTS
	WHERE CONSTRAINT_NAME = 'fk1_vineyards_vineyard_grape_id')
	ALTER TABLE vineyards DROP CONSTRAINT fk1_vineyards_vineyard_grape_id

IF EXISTS (SELECT * FROM INFORMATION_SCHEMA.TABLE_CONSTRAINTS
	WHERE CONSTRAINT_CATALOG = 'fk2_vineyards_vineyard_employee_id')
	ALTER TABLE vineyards DROP CONSTRAINT fk2_vineyards_vineyard_employee_id

DROP TABLE IF EXISTS vineyards
GO


IF EXISTS (SELECT * FROM INFORMATION_SCHEMA.TABLE_CONSTRAINTS
	WHERE CONSTRAINT_NAME = 'pk_fermenters_fermenter_id')
	ALTER TABLE fermenters DROP CONSTRAINT pk_fermenters_fermenter_id

	
IF EXISTS (SELECT * FROM INFORMATION_SCHEMA.TABLE_CONSTRAINTS
	WHERE CONSTRAINT_NAME = 'u1_fermenters_fermenter_number')
	ALTER TABLE fermenters DROP CONSTRAINT u1_fermenters_fermenter_number

IF EXISTS (SELECT * FROM INFORMATION_SCHEMA.TABLE_CONSTRAINTS
	WHERE CONSTRAINT_CATALOG = 'fk1_fermenters_fermenter_employee_id')
	ALTER TABLE fermenters DROP CONSTRAINT fk1_fermenters_fermenter_employee_id

DROP TABLE IF EXISTS fermenters
GO


IF EXISTS (SELECT * FROM INFORMATION_SCHEMA.TABLE_CONSTRAINTS
	WHERE CONSTRAINT_NAME = 'pk_grapes_grape_id')
	ALTER TABLE grapes DROP CONSTRAINT pk_grapes_grape_id

	
IF EXISTS (SELECT * FROM INFORMATION_SCHEMA.TABLE_CONSTRAINTS
	WHERE CONSTRAINT_NAME = 'u1_grapes_grape_name')
	ALTER TABLE grapes DROP CONSTRAINT u1_grapes_grape_name

DROP TABLE IF EXISTS grapes
GO


IF EXISTS (SELECT * FROM INFORMATION_SCHEMA.TABLE_CONSTRAINTS
	WHERE CONSTRAINT_NAME = 'pk_employees_employee_id')
	ALTER TABLE employees DROP CONSTRAINT pk_employees_employee_id

IF EXISTS (SELECT * FROM INFORMATION_SCHEMA.TABLE_CONSTRAINTS
	WHERE CONSTRAINT_NAME = 'u_employees_employee_email')
	ALTER TABLE employees DROP CONSTRAINT u_employees_employee_email

IF EXISTS (SELECT * FROM INFORMATION_SCHEMA.TABLE_CONSTRAINTS
	WHERE CONSTRAINT_NAME = 'fk_employees_employee_department')
	ALTER TABLE employees DROP CONSTRAINT fk_employees_employee_department
	
IF EXISTS (SELECT * FROM INFORMATION_SCHEMA.TABLE_CONSTRAINTS
	WHERE CONSTRAINT_NAME = 'fk_employees_employee_position')
	ALTER TABLE employees DROP CONSTRAINT fk_employees_employee_position

DROP TABLE IF EXISTS employees
GO


IF EXISTS (SELECT * FROM INFORMATION_SCHEMA.TABLE_CONSTRAINTS
	WHERE CONSTRAINT_NAME = 'pk_positions_position_code')
	ALTER TABLE positions DROP CONSTRAINT pk_positions_position_code
	
IF EXISTS (SELECT * FROM INFORMATION_SCHEMA.TABLE_CONSTRAINTS
	WHERE CONSTRAINT_NAME = 'u_positions_position_name')
	ALTER TABLE positions DROP CONSTRAINT u_positions_position_name

DROP TABLE IF EXISTS positions
GO


IF EXISTS (SELECT * FROM INFORMATION_SCHEMA.TABLE_CONSTRAINTS
	WHERE CONSTRAINT_NAME = 'pk_departments_department_code')
	ALTER TABLE departments DROP CONSTRAINT pk_departments_department_code
	
IF EXISTS (SELECT * FROM INFORMATION_SCHEMA.TABLE_CONSTRAINTS
	WHERE CONSTRAINT_NAME = 'u_departments_department_name')
	ALTER TABLE departments DROP CONSTRAINT u_departments_department_name

DROP TABLE IF EXISTS departments
GO




-------------------
-- UP (Metadata) --
-------------------

-- HR Tables
CREATE TABLE departments (
	department_code VARCHAR(50) NOT NULL -- pk
	, department_name VARCHAR(255) NOT NULL -- unique
	, CONSTRAINT pk_departments_department_code PRIMARY KEY (department_code)
	, CONSTRAINT u_departments_department_name UNIQUE (department_name)
)
GO

CREATE TABLE positions (
	position_code VARCHAR(50) NOT NULL -- pk
	, position_name VARCHAR(50) NOT NULL -- unique
	, position_pay_rate MONEY NOT NULL
	, CONSTRAINT pk_positions_position_code PRIMARY KEY (position_code)
	, CONSTRAINT u_positions_position_name UNIQUE (position_name)

)
GO

CREATE TABLE employees (
	employee_id INT IDENTITY(1,1) NOT NULL -- pk
	, employee_lastname VARCHAR(255) NOT NULL
	, employee_firstname VARCHAR(255) NOT NULL
	, employee_email VARCHAR(255) NOT NULL -- unique
	, employee_department VARCHAR(50) NOT NULL -- fk from departments table (department_code)
	, employee_position VARCHAR(50) NOT NULL -- fk from positions table (position_code)
	, CONSTRAINT pk_employees_employee_id PRIMARY KEY (employee_id)
	, CONSTRAINT u_employees_employee_email UNIQUE (employee_email)
	, CONSTRAINT fk_employees_employee_department FOREIGN KEY (employee_department) REFERENCES departments(department_code)
	, CONSTRAINT fk_employees_employee_position FOREIGN KEY (employee_position) REFERENCES positions(position_code)
)
GO


-- Operations tables

CREATE TABLE grapes ( --lookup
	grape_id INT IDENTITY(1,1) NOT NULL -- pk
	, grape_name VARCHAR(50) NOT NULL -- u1
	, CONSTRAINT pk_grapes_grape_id PRIMARY KEY (grape_id)
	, CONSTRAINT u1_grapes_grape_name UNIQUE (grape_name)
)
GO

CREATE TABLE fermenters ( -- lookup
	fermenter_id INT IDENTITY(1,1) NOT NULL -- pk
	, fermenter_number INT NOT NULL -- u
	, fermenter_employee_id INT NOT NULL
	, CONSTRAINT pk_fermenters_fermenter_id PRIMARY KEY (fermenter_id)
	, CONSTRAINT u1_fermenters_fermenter_number UNIQUE (fermenter_number)
	, CONSTRAINT fk1_fermenters_fermenter_employee_id FOREIGN KEY (fermenter_employee_id) REFERENCES employees(employee_id)
)
GO

CREATE TABLE vineyards ( --lookup
	vineyard_id INT IDENTITY(1,1) NOT NULL -- pk
	, vineyard_grape_id INT NOT NULL -- fk (grapes.grape_id)
	, vineyard_employee_id INT NOT NULL -- fk
	, CONSTRAINT pk_vineyards_vineyard_id PRIMARY KEY (vineyard_id)
	, CONSTRAINT fk1_vineyards_vineyard_grape_id FOREIGN KEY (vineyard_grape_id) REFERENCES grapes(grape_id)
	, CONSTRAINT fk2_vineyards_vineyard_employee_id FOREIGN KEY (vineyard_employee_id) REFERENCES employees(employee_id)
)
GO


/* 

Had to create the two following tables (fermenters_vineyards & vineyards_planting_harvests)
to properly store the interaction data between the vineyards, fermenters, and grapes tables 

*/
CREATE TABLE fermenters_vineyards (
	fermenter_batch_id INT IDENTITY(1,1) NOT NULL -- pk
	, fermenter_id INT NOT NULL -- fk1 (fermenters.fermenter_id)
	, fermenter_vineyard_id INT NOT NULL -- fk2 (vinyards.vinyard_id)
	, fermenter_start_date DATE NULL
	, fermenter_end_date DATE NULL
	, CONSTRAINT pk_fermenters_vineyards_fermenter_batch_id PRIMARY KEY (fermenter_batch_id)
	, CONSTRAINT fk1_fermenters_vineyards_fermenter_id FOREIGN KEY (fermenter_id) REFERENCES fermenters(fermenter_id)
	, CONSTRAINT fk2_fermenters_vineyards_fermenter_vineyard_id FOREIGN KEY (fermenter_vineyard_id) REFERENCES vineyards(vineyard_id)
)
GO

CREATE TABLE vineyards_planting_harvests (
	planting_harvest_id INT IDENTITY(1,1) NOT NULL -- pk
	, vineyard_id INT NOT NULL -- fk1 (vinyards.vineyard_id)
	, vineyard_section INT NOT NULL -- chk constraint (>=1 | <=6)
	, vineyard_plant_date DATE NULL -- chk constraint (< harvest date) 
	, vineyard_harvest_date DATE NULL -- chk constraint (> plant date)
	, CONSTRAINT pk_vineyards_planting_harvest_id PRIMARY KEY (planting_harvest_id)
	, CONSTRAINT chk1_vineyards_planting_harvest_vineyard_section CHECK (vineyard_section BETWEEN 1 AND 6)
	, CONSTRAINT fk1_vineyards_planting_harvests_vineyard_id FOREIGN KEY (vineyard_id) REFERENCES vineyards(vineyard_id)
	, CONSTRAINT chk2_vineyards_planting_harvests_vineyard_plant_date CHECK (vineyard_plant_date < vineyard_harvest_date)
	, CONSTRAINT chk3_vineyards_planting_harvests_vineyard_harvest_date CHECK (vineyard_harvest_date > vineyard_plant_date)
)
GO

-- Determined that it was more clear to name to columns this way...
CREATE TABLE cellars ( -- lookup
	cellar_id INT IDENTITY(1,1) NOT NULL -- pk
	, cellar_number INT NOT NULL
	, cellar_rack INT NOT NULL 
	, cellar_row INT NOT NULL
	, cellar_slot INT NOT NULL
	, CONSTRAINT pk_cellars_cellar_id PRIMARY KEY (cellar_id)
)
GO

-- Made more sense to use `cellar_id` instead of `barrel_id` here....  
-- because i think it makes more sense to show where barrels are stored in the barrels table, rather than showing what barrels are stored in the cellars table...
CREATE TABLE barrels (
	barrel_id INT IDENTITY(1,1) NOT NULL -- pk
	, barrel_number VARCHAR(10) NOT NULL -- u1
	, barrel_start_date DATE NOT NULL -- chk constraint (< end date)
	, barrel_end_date DATE NOT NULL -- chk constraint (> start date)
	, barrel_fermenter_id INT NOT NULL -- fk1 (fermenters.fermenter_id)
	, barrel_cellar_id INT NOT NULL -- fk2 (cellars.cellar_id)
	, CONSTRAINT pk_barrels_barrel_id PRIMARY KEY (barrel_id)
	, CONSTRAINT u1_barrels_barrel_number UNIQUE (barrel_number)
	, CONSTRAINT chk1_barrels_barrel_start_date CHECK (barrel_start_date < barrel_end_date)
	, CONSTRAINT chk2_barrels_barrel_end_date CHECK (barrel_end_date > barrel_start_date)
	, CONSTRAINT fk1_barrels_barrel_fermenter_id FOREIGN KEY (barrel_fermenter_id) REFERENCES fermenters(fermenter_id)
	, CONSTRAINT fk2_barrels_barrel_cellar_id FOREIGN KEY (barrel_cellar_id) REFERENCES cellars(cellar_id)
)
GO

CREATE TABLE products (
	product_id INT IDENTITY(1,1) NOT NULL -- pk
	, product_name VARCHAR(50) NOT NULL -- u1
	, product_price MONEY NOT NULL
	, product_desc VARCHAR(255) NOT NULL
	, CONSTRAINT pk_products_product_id PRIMARY KEY (product_id)
	, CONSTRAINT u1_products_product_name UNIQUE (product_name)
)
GO

CREATE TABLE bottles (
	bottle_id INT IDENTITY(1,1) NOT NULL -- pk 
	, bottle_label_number VARCHAR(50) NOT NULL -- u
	, bottle_fill_date DATE NOT NULL
	, bottle_barrel_id INT NOT NULL -- fk1 (barrels.barrel_id)
	, bottle_sold INT NOT NULL -- chk constr (== 1 | == 0)
	, bottle_product_id INT NOT NULL -- fk2 (products.product_id)
	, CONSTRAINT pk_bottles_bottle_id PRIMARY KEY (bottle_id)
	, CONSTRAINT u1_bottles_bottle_label_number UNIQUE (bottle_label_number)
	, CONSTRAINT fk1_bottles_bottle_barrel_id FOREIGN KEY (bottle_barrel_id) REFERENCES barrels(barrel_id)
	, CONSTRAINT chk1_bottles_bottle_sold CHECK (bottle_sold = 1 OR bottle_sold = 0)
	, CONSTRAINT fk2_bottles_bottle_product_id FOREIGN KEY (bottle_product_id) REFERENCES products(product_id)
)
GO


-- Sales Tables --

CREATE TABLE customers (
	customer_id INT IDENTITY(1,1) NOT NULL -- pk
	, customer_firstname VARCHAR(255) NOT NULL
	, customer_lastname VARCHAR(255) NOT NULL
	, customer_email VARCHAR(255) NOT NULL -- u1
	, customer_street_address VARCHAR(255) NOT NULL
	, customer_city VARCHAR(255) NOT NULL
	, customer_state VARCHAR(255) NOT NULL
	, customer_zipcode VARCHAR(11) NOT NULL
	, CONSTRAINT pk_customers_customer_id PRIMARY KEY (customer_id)
	, CONSTRAINT u1_customers_customer_email UNIQUE (customer_email)
)
GO

CREATE TABLE shippers (
	shipper_id INT IDENTITY(1,1) NOT NULL -- pk
	, shipper_name VARCHAR(255) NOT NULL -- u
	, shipper_phone_number VARCHAR(10) NOT NULL -- u
	, CONSTRAINT pk_shippers_shipper_id PRIMARY KEY (shipper_id)
	, CONSTRAINT u1_shippers_shipper_name UNIQUE (shipper_name)
	, CONSTRAINT u2_shippers_shipper_phone_number UNIQUE (shipper_phone_number)
)
GO

CREATE TABLE orders (
	order_id INT IDENTITY(1,1) NOT NULL -- pk
	, order_bottle_id INT NOT NULL -- fk1 (bottles.bottle_id)
	, order_customer_id INT NOT NULL -- fk2 (customers.customer_id)
	, order_shipper_id INT NOT NULL -- fk3 (shippers.shipper_id)
	, order_quantity INT NOT NULL
	, order_date DATE NOT NULL
	, CONSTRAINT pk_orders_order_id PRIMARY KEY (order_id)
	, CONSTRAINT fk1_orders_order_bottle_id FOREIGN KEY (order_bottle_id) REFERENCES bottles(bottle_id)
	, CONSTRAINT fk2_orders_order_customer_id FOREIGN KEY (order_customer_id) REFERENCES customers(customer_id)
	, CONSTRAINT fk3_orders_order_shipper_id FOREIGN KEY (order_shipper_id) REFERENCES shippers(shipper_id)
)
GO

CREATE TABLE payments (
	payment_id INT IDENTITY(1,1) NOT NULL -- pk
	, payment_customer_id INT NOT NULL -- fk1 (customers.customer_id)
	, payment_ccard_num VARCHAR(16) NOT NULL -- u
	, payment_ccard_exp DATE NOT NULL
	, payment_ccard_ccv VARCHAR(3) NOT NULL
	, payment_ccard_type VARCHAR(50) NOT NULL
	, CONSTRAINT pk_payments_payment_id PRIMARY KEY (payment_id)
	, CONSTRAINT fk1_payments_payment_customer_id FOREIGN KEY (payment_customer_id) REFERENCES customers(customer_id)
	, CONSTRAINT u1_payments_payment_ccard_num UNIQUE (payment_ccard_num)
)
GO




---------------
-- UP (DATA) --
---------------

-- HR Tables
INSERT INTO departments 
	VALUES
		('7264', 'Executive'				)
		, ('8190', 'Human Resources'		)
		, ('9293', 'Operations'				)
		, ('3735', 'Sales and Marketing'	)
		, ('2263', 'Laboratory and Research')
		, ('8905', 'Agriculture'			)
		, ('2005', 'Supply Chain Managment'	)
		, ('9881', 'Analytics'				)
GO


INSERT INTO positions
	VALUES
		('7264-1', 'President', 100)
		, ('8190-1', 'Chief People Officer', 75)
		, ('8190-2', 'Manager - Human Resources', 60)
		, ('8190-3', 'Associate - Human Resources', 40)
		, ('8190-4', 'Analyst - Human Resources', 50)
		, ('9293-1', 'Cheif Operations Officer', 75)
		, ('9293-2', 'Manager - Operations', 60)
		, ('9293-3', 'Analyst - Operations', 50)
		, ('3735-1', 'Chief Sales and Marketing Officer', 75)
		, ('3735-2', 'Manager - Sales', 60)
		, ('3735-3', 'Manager - Marketing', 60)
		, ('3735-4', 'Associate - Sales', 40)
		, ('3735-5', 'Associate - Marketing', 40)
		, ('3735-6', 'Analyst - Sales and Marketing', 50)
		, ('2263-1', 'Cheif Research Officer', 75)
		, ('2263-2', 'Manager - Lab Testing', 60)
		, ('2263-3', 'Manager - Research', 60)
		, ('2263-4', 'Scientist - Research and Development', 65)
		, ('2263-5', 'Laboratory Tech', 40)
		, ('2263-6', 'Analyst - Research and Development', 50)
		, ('8905-1', 'Manager - Agriculture', 60)
		, ('8905-2', 'Equipment Operator', 40)
		, ('8905-3', 'Mechanic - Heavy Equipment', 40)
		, ('8905-4', 'Mechanic - Small Equipment', 40)
		, ('2005-1', 'Director - Supply Chain Management', 70)
		, ('2005-2', 'Manager - Warhousing', 60)
		, ('2005-3', 'Manager - Shipping', 60)
		, ('2005-4', 'Associate - Supply Chain', 40)
		, ('2005-5', 'Analyst - Supply Chain', 50)
		, ('9881-1', 'Chief Analytics Officer', 75)
		, ('9881-2', 'Analytics Manager - Sales and Marketing', 60)
		, ('9881-3', 'Analytics Manager - Operations', 60)
		, ('9881-4', 'Analytics Manager - Human Resources', 60)
		, ('9881-5', 'Analytics Manager - Research and Development', 60)
		, ('9881-6', 'Analytics Manager - Agriculture', 60)
		, ('9881-7', 'Analytics Manager - Supply Chain', 60)
GO


INSERT INTO employees	
	VALUES
		('de Chateau Cuse', 'Prezzy', 'prezzy.dechateaucuse@chateaucuse.org', '7264', '7264-1') 
		, ('Smith', 'Matt', 'matt.smith@chateaucuse.org', '8190', '8190-1')
		, ('Stabel', 'Sintia', 'sintia.stabel@chateaucuse.org', '3735', '3735-1')
		, ('Damico', 'Bryan', 'bryan.damico@chateaucuse.org', '9293', '9293-1')
		, ('Roberts','Uriel','u.roberts@chateaucuse.org','2263','2263-6')
		, ('Gallegos','Xyla','g_xyla@chateaucuse.org','9881','9881-1')
		, ('Fleming','Piper','fleming_piper809@chateaucuse.org','8190','8190-3')
		, ('Monroe','Teagan','teagan-monroe@chateaucuse.org','9881','9881-3')
		, ('Hess','Julie','h_julie@chateaucuse.org','9293','9293-3')
		, ('Collins','Mark','markcollins@chateaucuse.org','2005','2005-5')
		, ('Wolf','Vaughan','w-vaughan9300@chateaucuse.org','2005','2005-3')
		, ('Beard','Malcolm','bmalcolm6433@chateaucuse.org','8905','8905-3')
		, ('Avery','Melvin','m.avery1826@chateaucuse.org','2263','2263-3')
		, ('Everett','Caryn','e-caryn1429@chateaucuse.org','9881','9881-2')
		, ('Cooke','Quamar','quamarcooke@chateaucuse.org','9881','9881-5')
		, ('Valdez','Clarke','clarke-valdez@chateaucuse.org','3735','3735-4')
		, ('Curry','Rylee','c.rylee6926@chateaucuse.org','2005','2005-2')
		, ('Delaney','Hu','d.hu8553@chateaucuse.org','9881','9881-2')
		, ('Ball','Mason','masonball7238@chateaucuse.org','2005','2005-3')
		, ('Glass','Rylee','rylee_glass3763@chateaucuse.org','9881','9881-7')
		, ('Rasmussen','Cain','cain-rasmussen@chateaucuse.org','3735','3735-3')
		, ('Guy','Jesse','jesse.guy2752@chateaucuse.org','9881','9881-3')
		, ('Morgan','Thane','morgan.thane9944@chateaucuse.org','2005','2005-2')
		, ('Landry','Phyllis','phyllis-landry@chateaucuse.org','8190','8190-4')
		, ('Rodriguez','Porter','r_porter@chateaucuse.org','9293','9293-3')
		, ('Merrill','Hope','m.hope@chateaucuse.org','2005','2005-2')
		, ('Flynn','Brynne','f-brynne@chateaucuse.org','9881','9881-6')
		, ('Barker','Matthew','b_matthew@chateaucuse.org','9881','9881-6')
		, ('Alston','Xanthus','axanthus8347@chateaucuse.org','8190','8190-4')
		, ('Cline','Silas','silas-cline1290@chateaucuse.org','9881','9881-3')
		, ('Solomon','Amethyst','samethyst@chateaucuse.org','9881','9293-3')
		, ('Finch','Gail','f.gail2047@chateaucuse.org','2263','2263-3')
		, ('Leblanc','Aurelia','a_leblanc@chateaucuse.org','9881','9293-3')
		, ('Thornton','Whilemina','w.thornton@chateaucuse.org','2005','2005-1')
		, ('Thornton','Aladdin','aladdin.thornton2226@chateaucuse.org','9293','9293-3')
		, ('Singleton','Fleur','singletonfleur@chateaucuse.org','3735','3735-4')
		, ('Morgan','Isaac','morgan-isaac9858@chateaucuse.org','3735','3735-4')
		, ('Parks','Brennan','brennan-parks77@chateaucuse.org','9881','9881-5')
		, ('Hensley','Adam','hadam1719@chateaucuse.org','2263','2263-5')
		, ('Hurley','Desirae','hurley-desirae8136@chateaucuse.org','2263','2263-5')
		, ('Casey','Joan','j.casey@chateaucuse.org','8905','8905-2')
		, ('Valenzuela','Amal','v-amal1047@chateaucuse.org','8905','8905-3')
		, ('Emerson','Gretchen','e-gretchen@chateaucuse.org','2005','2005-1')
		, ('Holder','Gemma','gemma-holder@chateaucuse.org','3735','3735-6')
		, ('Steele','Zoe','z.steele2340@chateaucuse.org','3735','3735-4')
		, ('Abbott','Eve','e.abbott@chateaucuse.org','2005','2005-1')
		, ('Chambers','Steven','chambers_steven@chateaucuse.org','3735','3735-4')
		, ('Potter','Lars','p.lars@chateaucuse.org','8905','8905-2')
		, ('Marquez','Bradley','bradley.marquez@chateaucuse.org','9881','9881-6')
		, ('Duran','Oprah','o_duran2756@chateaucuse.org','2263','2263-3')
		, ('Coleman','Drake','drake.coleman7999@chateaucuse.org','9881','9881-2')
		, ('Peters','Rhiannon','r_peters2991@chateaucuse.org','9881','9881-6')
		, ('Wilkerson','Lysandra','wilkersonlysandra@chateaucuse.org','3735','3735-5')
		, ('Love','Nola','l.nola9817@chateaucuse.org','9881','9881-6')
GO


-- Operations Tables

INSERT INTO grapes
	VALUES
		('Chardonnay'		)
		, ('Riesling'		)
		, ('Pinot Noir'		)
		, ('Merlot'			)
		, ('Savignon Blanc'	)
		, ('Pinot Grigio'	)


INSERT INTO fermenters
	VALUES
		(1	, 12)
		,(2	, 41)
		,(3	, 42)
		,(4	, 48)
		,(5	, 12)
		,(6	, 41)
		,(7	, 42)
		,(8	, 48)


INSERT INTO vineyards
	VALUES
		(1, 41)
		, (2, 41)
		, (3, 41)
		, (4, 48)
		, (5, 48)
		, (6, 48)
		
INSERT INTO fermenters_vineyards
	VALUES
		(1,	1,	'2014-10-03',	'2014-10-19')
		, (2,	1,	'2014-10-05',	'2014-10-21')
		, (3,	2,	'2014-10-05',	'2014-10-21')
		, (4,	4,	'2014-10-06',	'2014-10-24')
		, (5,	3,	'2015-10-07',	'2015-10-23')
		, (6,	5,	'2015-10-07',	'2015-10-26')
		, (7,	5,	'2015-10-08',	'2015-10-29')
		, (8,	6,	'2015-10-10',	'2015-10-30')
		, (1,	1,	'2018-10-03',	'2018-10-25')
		, (2,	1,	'2018-10-04',	'2018-10-26')
		, (3,	2,	'2018-10-05',	'2018-10-27')
		, (4,	4,	'2018-10-09',	'2018-10-28')
		, (5,	3,	'2019-09-29',	'2019-10-20')
		, (6,	5,	'2019-10-02',	'2019-10-26')
		, (7,	5,	'2019-10-02',	'2019-10-26')
		, (8,	6,	'2019-10-05',	'2019-10-28')
		, (1,	1,	'2021-10-03',	'2021-10-25')
		, (2,	1,	'2021-10-04',	'2021-10-25')
		, (3,	2,	'2021-10-05',	'2021-10-26')
		, (4,	4,	'2021-10-06',	'2021-10-29')


INSERT INTO vineyards_planting_harvests
	VALUES
		(1,	1,	'2011-04-05',	'2014-10-02')
		,(2,	1,	'2011-04-07',	'2014-10-04')
		,(3,	1,	'2011-04-08',	'2014-10-06')
		,(4,	1,	'2012-04-01',	'2015-09-28')
		,(5,	1,	'2012-04-04',	'2015-10-01')
		,(6,	1,	'2012-04-07',	'2015-10-04')
		,(1,	2,	'2015-04-05',	'2018-10-01')
		,(2,	2,	'2015-04-06',	'2018-10-03')
		,(3,	2,	'2015-04-09',	'2018-10-08')
		,(4,	2,	'2016-04-01',	'2019-09-28')
		,(5,	2,	'2016-04-05',	'2019-10-01')
		,(6,	2,	'2016-04-07',	'2019-10-04')
		,(1,	3,	'2019-04-03',	'2021-10-02')
		,(2,	3,	'2019-04-06',	'2021-10-04')
		,(3,	3,	'2019-04-07',	'2021-10-05')
		,(4,	3,	'2020-04-10',	NULL		)
		,(5,	3,	'2020-04-12',	NULL		)
		,(6,	3,	'2020-04-13',	NULL		)

INSERT INTO cellars
	VALUES
	(1,	1,	1,	1)
	,(1, 1,	1,	2)
	,(1, 1,	1,	3)
	,(1, 1,	1,	4)
	,(1, 1,	1,	5)
	,(1, 1,	1,	6)
	,(1, 1,	1,	7)
	,(1, 1,	1,	8)
	,(1, 1,	2,	1)
	,(1, 1,	2,	2)
	,(1, 1,	2,	3)
	,(1, 1,	2,	4)
	,(1, 1,	2,	5)
	,(1, 1,	2,	6)
	,(1, 1,	2,	7)
	,(1, 1,	2,	8)
	,(1, 2,	1,	1)
	,(1, 2,	1,	2)
	,(1, 2,	1,	3)
	,(1, 2,	1,	4)
	,(1, 2,	1,	5)
	,(1, 2,	1,	6)
	,(1, 2,	1,	7)
	,(1, 2,	1,	8)
	,(1, 2,	2,	1)
	,(1, 2,	2,	2)
	,(1, 2,	2,	3)
	,(1, 2,	2,	4)
	,(1, 2,	2,	5)
	,(1, 2,	2,	6)
	,(1, 2,	2,	7)
	,(1, 2,	2,	8)
	,(2, 1,	1,	1)
	,(2, 1,	1,	2)
	,(2, 1,	1,	3)
	,(2, 1,	1,	4)
	,(2, 1,	1,	5)
	,(2, 1,	1,	6)
	,(2, 1,	1,	7)
	,(2, 1,	1,	8)
	,(2, 1,	2,	1)
	,(2, 1,	2,	2)
	,(2, 1,	2,	3)
	,(2, 1,	2,	4)
	,(2, 1,	2,	5)
	,(2, 1,	2,	6)
	,(2, 1,	2,	7)
	,(2, 1,	2,	8)
	,(2, 2,	1,	1)
	,(2, 2,	1,	2)
	,(2, 2,	1,	3)
	,(2, 2,	1,	4)
	,(2, 2,	1,	5)
	,(2, 2,	1,	6)
	,(2, 2,	1,	7)
	,(2, 2,	1,	8)
	,(2, 2,	2,	1)
	,(2, 2,	2,	2)
	,(2, 2,	2,	3)
	,(2, 2,	2,	4)
	,(2, 2,	2,	5)
	,(2, 2,	2,	6)
	,(2, 2,	2,	7)
	,(2, 2,	2,	8)
	,(3, 1,	1,	1)
	,(3, 1,	1,	2)
	,(3, 1,	1,	3)
	,(3, 1,	1,	4)
	,(3, 1,	1,	5)
	,(3, 1,	1,	6)
	,(3, 1,	1,	7)
	,(3, 1,	1,	8)
	,(3, 1,	2,	1)
	,(3, 1,	2,	2)
	,(3, 1,	2,	3)
	,(3, 1,	2,	4)
	,(3, 1,	2,	5)
	,(3, 1,	2,	6)
	,(3, 1,	2,	7)
	,(3, 1,	2,	8)
	,(3, 2,	1,	1)
	,(3, 2,	1,	2)
	,(3, 2,	1,	3)
	,(3, 2,	1,	4)
	,(3, 2,	1,	5)
	,(3, 2,	1,	6)
	,(3, 2,	1,	7)
	,(3, 2,	1,	8)
	,(3, 2,	2,	1)
	,(3, 2,	2,	2)
	,(3, 2,	2,	3)
	,(3, 2,	2,	4)
	,(3, 2,	2,	5)
	,(3, 2,	2,	6)
	,(3, 2,	2,	7)
	,(3, 2,	2,	8)
	,(4, 1,	1,	1)
	,(4, 1,	1,	2)
	,(4, 1,	1,	3)
	,(4, 1,	1,	4)
	,(4, 1,	1,	5)
	,(4, 1,	1,	6)
	,(4, 1,	1,	7)
	,(4, 1,	1,	8)
	,(4, 1,	2,	1)
	,(4, 1,	2,	2)
	,(4, 1,	2,	3)
	,(4, 1,	2,	4)
	,(4, 1,	2,	5)
	,(4, 1,	2,	6)
	,(4, 1,	2,	7)
	,(4, 1,	2,	8)
	,(4, 2,	1,	1)
	,(4, 2,	1,	2)
	,(4, 2,	1,	3)
	,(4, 2,	1,	4)
	,(4, 2,	1,	5)
	,(4, 2,	1,	6)
	,(4, 2,	1,	7)
	,(4, 2,	1,	8)
	,(4, 2,	2,	1)
	,(4, 2,	2,	2)
	,(4, 2,	2,	3)
	,(4, 2,	2,	4)
	,(4, 2,	2,	5)
	,(4, 2,	2,	6)
	,(4, 2,	2,	7)
	,(4, 2,	2,	8)


INSERT INTO barrels
	VALUES
		('C1'	,'2014-10-20',	'2015-04-20',	1,	1  ) 
		,('C2'	,'2014-10-20',	'2015-04-20',	1,	2  )
		,('C3'	,'2014-10-20',	'2015-04-20',	1,	3  )
		,('C4'	,'2014-10-20',	'2015-04-20',	1,	4  )
		,('C5'	,'2014-10-20',	'2015-04-20',	1,	5  )
		,('C6'	,'2014-10-20',	'2015-04-20',	1,	6  )
		,('C7'	,'2014-10-20',	'2015-04-20',	1,	7  )
		,('C8'	,'2014-10-20',	'2015-04-20',	1,	8  )
		,('C9'	,'2014-10-20',	'2015-04-20',	1,	9  )
		,('C10'  ,'2014-10-20',	'2015-04-20',	1,	10 )
		,('C11'  ,'2014-10-22',	'2015-04-20',	2,	11 )
		,('C12'  ,'2014-10-22',	'2015-04-20',	2,	12 )
		,('C13'  ,'2014-10-22',	'2015-04-20',	2,	13 )
		,('C14'  ,'2014-10-22',	'2015-04-20',	2,	14 )
		,('C15'  ,'2014-10-22',	'2015-04-20',	2,	15 )
		,('C16'  ,'2014-10-22',	'2015-04-20',	2,	16 )
		,('C17'  ,'2014-10-22',	'2015-04-20',	2,	17 )
		,('C18'  ,'2014-10-22',	'2015-04-20',	2,	18 )
		,('C19'  ,'2014-10-22',	'2015-04-20',	2,	19 )
		,('C20'  ,'2014-10-22',	'2015-04-20',	2,	20 )
		,('R1'	,'2014-10-22',	'2015-06-22',	3,	21 )
		,('R2'	,'2014-10-22',	'2015-06-22',	3,	22 )
		,('R3'	,'2014-10-22',	'2015-06-22',	3,	23 )
		,('R4'	,'2014-10-22',	'2015-06-22',	3,	24 )
		,('R5'	,'2014-10-22',	'2015-06-22',	3,	25 )
		,('R6'	,'2014-10-22',	'2015-06-22',	3,	26 )
		,('R7'	,'2014-10-22',	'2015-06-22',	3,	27 )
		,('R8'	,'2014-10-22',	'2015-06-22',	3,	28 )
		,('R9'	,'2014-10-22',	'2015-06-22',	3,	29 )
		,('R10'  ,'2014-10-22',	'2015-06-22',	3,	30 )
		,('M1'	,'2014-10-24',	'2016-08-24',	4,	31 )
		,('M2'	,'2014-10-24',	'2016-08-24',	4,	32 )
		,('M3'	,'2014-10-24',	'2016-08-24',	4,	33 )
		,('M4'	,'2014-10-24',	'2016-08-24',	4,	34 )
		,('M5'	,'2014-10-24',	'2016-08-24',	4,	35 )
		,('M6'	,'2014-10-24',	'2016-08-24',	4,	36 )
		,('M7'	,'2014-10-24',	'2016-08-24',	4,	37 )
		,('M8'	,'2014-10-24',	'2016-08-24',	4,	38 )
		,('M9'	,'2014-10-24',	'2016-08-24',	4,	39 )
		,('M10'  ,'2014-10-24',	'2016-08-24',	4,	40 )
		,('PN1'  ,'2015-10-23',	'2017-06-23',	5,	41 )
		,('PN2'  ,'2015-10-23',	'2017-06-23',	5,	42 )
		,('PN3'  ,'2015-10-23',	'2017-06-23',	5,	43 )
		,('PN4'  ,'2015-10-23',	'2017-06-23',	5,	44 )
		,('PN5'  ,'2015-10-23',	'2017-06-23',	5,	45 )
		,('PN6'  ,'2015-10-23',	'2017-06-23',	5,	46 )
		,('PN7'  ,'2015-10-23',	'2017-06-23',	5,	47 )
		,('PN8'  ,'2015-10-23',	'2017-06-23',	5,	48 )
		,('PN9'  ,'2015-10-23',	'2017-06-23',	5,	49 )
		,('PN10' ,'2015-10-23',	'2017-06-23',	5,	50 )
		,('SB1'  ,'2015-10-26',	'2016-04-28',	6,	51 )
		,('SB2'  ,'2015-10-26',	'2016-04-28',	6,	52 )
		,('SB3'  ,'2015-10-26',	'2016-04-28',	6,	53 )
		,('SB4'  ,'2015-10-26',	'2016-04-28',	6,	54 )
		,('SB5'  ,'2015-10-26',	'2016-04-28',	6,	55 )
		,('SB6'  ,'2015-10-26',	'2016-04-28',	6,	56 )
		,('SB7'  ,'2015-10-26',	'2016-04-28',	6,	57 )
		,('SB8'  ,'2015-10-26',	'2016-04-28',	6,	58 )
		,('SB9'  ,'2015-10-26',	'2016-04-28',	6,	59 )
		,('SB10' ,'2015-10-26',	'2016-04-28',	6,	60 )
		,('SB11' ,'2015-10-29',	'2016-04-28',	7,	61 )
		,('SB12' ,'2015-10-29',	'2016-04-28',	7,	62 )
		,('SB13' ,'2015-10-29',	'2016-04-28',	7,	63 )
		,('SB14' ,'2015-10-29',	'2016-04-28',	7,	64 )
		,('SB15' ,'2015-10-29',	'2016-04-28',	7,	65 )
		,('SB16' ,'2015-10-29',	'2016-04-28',	7,	66 )
		,('SB17' ,'2015-10-29',	'2016-04-28',	7,	67 )
		,('SB18' ,'2015-10-29',	'2016-04-28',	7,	68 )
		,('SB19' ,'2015-10-29',	'2016-04-28',	7,	69 )
		,('SB20' ,'2015-10-29',	'2016-04-28',	7,	70 )
		,('PG1'  ,'2015-10-30',	'2016-06-28',	8,	71 )
		,('PG2'  ,'2015-10-30',	'2016-06-28',	8,	72 )
		,('PG3'  ,'2015-10-30',	'2016-06-28',	8,	73 )
		,('PG4'  ,'2015-10-30',	'2016-06-28',	8,	74 )
		,('PG5'  ,'2015-10-30',	'2016-06-28',	8,	75 )
		,('PG6'  ,'2015-10-30',	'2016-06-28',	8,	76 )
		,('PG7'  ,'2015-10-30',	'2016-06-28',	8,	77 )
		,('PG8'  ,'2015-10-30',	'2016-06-28',	8,	78 )
		,('PG9'  ,'2015-10-30',	'2016-06-28',	8,	79 )
		,('PG10' ,'2015-10-30',	'2016-06-28',	8,	80 )
		,('C21'  ,'2018-10-25',	'2019-04-23',	1,	81 )
		,('C22'  ,'2018-10-25',	'2019-04-23',	1,	82 )
		,('C23'  ,'2018-10-25',	'2019-04-23',	1,	83 )
		,('C24'  ,'2018-10-25',	'2019-04-23',	1,	84 )
		,('C25'  ,'2018-10-25',	'2019-04-23',	1,	85 )
		,('C26'  ,'2018-10-25',	'2019-04-23',	1,	86 )
		,('C27'  ,'2018-10-25',	'2019-04-23',	1,	87 )
		,('C28'  ,'2018-10-25',	'2019-04-23',	1,	88 )
		,('C29'  ,'2021-10-25',	'2022-04-28',	1,	89 )
		,('C30'  ,'2021-10-25',	'2022-04-28',	1,	90 )
		,('C31'  ,'2021-10-25',	'2022-04-28',	2,	91 )
		,('C32'  ,'2021-10-25',	'2022-04-28',	2,	92 )
		,('C33'  ,'2021-10-25',	'2022-04-28',	2,	93 )
		,('C34'  ,'2021-10-25',	'2022-04-28',	2,	94 )
		,('C35'  ,'2021-10-25',	'2022-04-28',	2,	95 )
		,('C36'  ,'2021-10-25',	'2022-04-28',	2,	96 )
		,('C37'  ,'2021-10-25',	'2022-04-28',	2,	97 )
		,('C38'  ,'2021-10-25',	'2022-04-28',	2,	98 )
		,('C39'  ,'2021-10-25',	'2022-04-28',	2,	99 )
		,('C40'  ,'2021-10-25',	'2022-04-28',	2,	100)
		,('R11'  ,'2021-10-25',	'2022-04-28',	3,	101)
		,('R12'  ,'2021-10-25',	'2022-04-28',	3,	102)
		,('R13'  ,'2021-10-25',	'2022-04-28',	3,	103)
		,('R14'  ,'2021-10-25',	'2022-04-28',	3,	104)
		,('R15'  ,'2021-10-25',	'2022-04-28',	3,	105)
		,('R16'  ,'2021-10-25',	'2022-04-28',	3,	106)
		,('R17'  ,'2021-10-25',	'2022-04-28',	3,	107)
		,('R18'  ,'2021-10-25',	'2022-04-28',	3,	108)
		,('R19'  ,'2021-10-26',	'2022-06-10',	3,	109)
		,('R20'  ,'2021-10-26',	'2022-06-10',	3,	110)
		,('M11'  ,'2021-10-26',	'2022-06-10',	4,	111)
		,('M12'  ,'2021-10-26',	'2022-06-10',	4,	112)
		,('M13'  ,'2021-10-26',	'2022-06-10',	4,	113)
		,('M14'  ,'2021-10-26',	'2022-06-10',	4,	114)
		,('M15'  ,'2021-10-26',	'2022-06-10',	4,	115)
		,('M16'  ,'2021-10-26',	'2022-06-10',	4,	116)
		,('M17'  ,'2021-10-26',	'2022-06-10',	4,	117)
		,('M18'  ,'2021-10-26',	'2022-06-10',	4,	118)
		,('M19'  ,'2021-10-30',	'2023-08-15',	4,	119)
		,('M20'  ,'2021-10-30',	'2023-08-15',	4,	120)
		,('PN11' ,'2021-10-30',	'2023-08-15',	5,	121)
		,('PN12' ,'2021-10-30',	'2023-08-15',	5,	122)
		,('PN13' ,'2021-10-30',	'2023-08-15',	5,	123)
		,('PN14' ,'2021-10-30',	'2023-08-15',	5,	124)
		,('PN15' ,'2021-10-30',	'2023-08-15',	5,	125)
		,('PN16' ,'2021-10-30',	'2023-08-15',	5,	126)
		,('PN17' ,'2021-10-30',	'2023-08-15',	5,	127)
		,('PN18' ,'2021-10-30',	'2023-08-15',	5,	128)


INSERT INTO products
	VALUES
	('Chardonnay',		8.5,	'white')
	,('Riesling',		10.5,	'white')
	,('Pinot Noir',		11.6,	'red'  )
	,('Merlot',			13.2,	'red'  )
	,('Savignon Blanc',	12.5,	'white')
	,('Pinot Grigio',	15.9,	'white')


INSERT INTO bottles
	VALUES
	('C1-1',	'2015-04-20',	1,	    1,	1)
	,('C1-2',	'2015-04-20',	1,	    1,	1)
	,('C1-3',	'2015-04-20',	1,	    1,	1)
	,('C1-4',	'2015-04-20',	1,	    1,	1)
	,('C1-5',	'2015-04-20',	1,	    1,	1)
	,('C2-1',	'2015-04-20',	2,	    1,	1)
	,('C2-2',	'2015-04-20',	2,	    1,	1)
	,('C2-3',	'2015-04-20',	2,	    1,	1)
	,('C2-4',	'2015-04-20',	2,	    1,	1)
	,('C2-5',	'2015-04-20',	2,	    1,	1)
	,('C3-1',	'2015-04-20',	3,	    1,	1)
	,('C3-2',	'2015-04-20',	3,	    1,	1)
	,('C3-3',	'2015-04-20',	3,	    1,	1)
	,('C3-4',	'2015-04-20',	3,	    1,	1)
	,('C3-5',	'2015-04-20',	3,	    1,	1)
	,('C4-1',	'2015-04-20',	4,	    1,	1)
	,('C4-2',	'2015-04-20',	4,	    1,	1)
	,('C4-3',	'2015-04-20',	4,	    1,	1)
	,('C4-4',	'2015-04-20',	4,	    1,	1)
	,('C4-5',	'2015-04-20',	4,	    1,	1)
	,('C5-1',	'2015-04-20',	5,	    1,	1)
	,('C5-2',	'2015-04-20',	5,	    1,	1)
	,('C5-3',	'2015-04-20',	5,	    1,	1)
	,('C5-4',	'2015-04-20',	5,	    1,	1)
	,('C5-5',	'2015-04-20',	5,	    1,	1)
	,('C7-1',	'2015-04-20',	7,	    1,	1)
	,('C7-2',	'2015-04-20',	7,	    1,	1)
	,('C7-3',	'2015-04-20',	7,	    1,	1)
	,('C7-4',	'2015-04-20',	7,	    1,	1)
	,('C7-5',	'2015-04-20',	7,	    1,	1)
	,('C8-1',	'2015-04-20',	8,	    1,	1)
	,('C8-2',	'2015-04-20',	8,	    1,	1)
	,('C8-3',	'2015-04-20',	8,	    1,	1)
	,('C8-4',	'2015-04-20',	8,	    1,	1)
	,('C8-5',	'2015-04-20',	8,	    1,	1)
	,('C9-1',	'2015-04-20',	9,	    1,	1)
	,('C9-2',	'2015-04-20',	9,	    1,	1)
	,('C9-3',	'2015-04-20',	9,	    1,	1)
	,('C9-4',	'2015-04-20',	9,	    1,	1)
	,('C9-5',	'2015-04-20',	9,	    1,	1)
	,('C10-1',	'2015-04-20',	10, 	1,	1)
	,('C10-2',	'2015-04-20',	10, 	1,	1)
	,('C10-3',	'2015-04-20',	10, 	1,	1)
	,('C10-4',	'2015-04-20',	10, 	1,	1)
	,('C10-5',	'2015-04-20',	10, 	1,	1)
	,('C11-1',	'2015-04-20',	11, 	1,	1)
	,('C11-2',	'2015-04-20',	11, 	1,	1)
	,('C11-3',	'2015-04-20',	11, 	1,	1)
	,('C11-4',	'2015-04-20',	11, 	1,	1)
	,('C11-5',	'2015-04-20',	11, 	1,	1)
	,('C12-1',	'2015-04-20',	12, 	1,	1)
	,('C12-2',	'2015-04-20',	12, 	0,	1)
	,('C12-3',	'2015-04-20',	12, 	0,	1)
	,('C12-4',	'2015-04-20',	12, 	0,	1)
	,('C12-5',	'2015-04-20',	12, 	0,	1)
	,('C13-1',	'2015-04-20',	13, 	1,	1)
	,('C13-2',	'2015-04-20',	13, 	1,	1)
	,('C13-3',	'2015-04-20',	13, 	1,	1)
	,('C13-4',	'2015-04-20',	13, 	1,	1)
	,('C13-5',	'2015-04-20',	13, 	1,	1)
	,('C14-1',	'2015-04-20',	14, 	1,	1)
	,('C14-2',	'2015-04-20',	14, 	1,	1)
	,('C14-3',	'2015-04-20',	14, 	1,	1)
	,('C14-4',	'2015-04-20',	14, 	1,	1)
	,('C14-5',	'2015-04-20',	14, 	1,	1)
	,('C15-1',	'2015-04-20',	15, 	1,	1)
	,('C15-2',	'2015-04-20',	15, 	1,	1)
	,('C15-3',	'2015-04-20',	15, 	1,	1)
	,('C15-4',	'2015-04-20',	15, 	1,	1)
	,('C15-5',	'2015-04-20',	15, 	1,	1)
	,('C16-1',	'2015-04-20',	16, 	1,	1)
	,('C16-2',	'2015-04-20',	16, 	1,	1)
	,('C16-3',	'2015-04-20',	16, 	1,	1)
	,('C16-4',	'2015-04-20',	16, 	1,	1)
	,('C16-5',	'2015-04-20',	16, 	1,	1)
	,('C17-1',	'2015-04-20',	17, 	1,	1)
	,('C17-2',	'2015-04-20',	17, 	1,	1)
	,('C17-3',	'2015-04-20',	17, 	1,	1)
	,('C17-4',	'2015-04-20',	17, 	1,	1)
	,('C17-5',	'2015-04-20',	17, 	1,	1)
	,('C18-1',	'2015-04-20',	18, 	1,	1)
	,('C18-2',	'2015-04-20',	18, 	1,	1)
	,('C18-3',	'2015-04-20',	18, 	1,	1)
	,('C18-4',	'2015-04-20',	18, 	1,	1)
	,('C18-5',	'2015-04-20',	18, 	1,	1)
	,('C19-1',	'2015-04-20',	19, 	1,	1)
	,('C19-2',	'2015-04-20',	19, 	1,	1)
	,('C19-3',	'2015-04-20',	19, 	1,	1)
	,('C19-4',	'2015-04-20',	19, 	0,	1)
	,('C19-5',	'2015-04-20',	19, 	0,	1)
	,('C20-1',	'2015-04-20',	20, 	0,	1)
	,('C20-2',	'2015-04-20',	20, 	1,	1)
	,('C20-3',	'2015-04-20',	20, 	1,	1)
	,('C20-4',	'2015-04-20',	20, 	1,	1)
	,('C20-5',	'2015-04-20',	20, 	1,	1)
	,('R1-1',	'2015-06-22',	21, 	1,	2)
	,('R1-2',	'2015-06-22',	21, 	1,	2)
	,('R1-3',	'2015-06-22',	21, 	1,	2)
	,('R1-4',	'2015-06-22',	21, 	1,	2)
	,('R1-5',	'2015-06-22',	22, 	1,	2)
	,('R2-1',	'2015-06-22',	22, 	1,	2)
	,('R2-2',	'2015-06-22',	22, 	1,	2)
	,('R2-3',	'2015-06-22',	22, 	1,	2)
	,('R2-4',	'2015-06-22',	22, 	0,	2)
	,('R2-5',	'2015-06-22',	22, 	0,	2)
	,('R3-1',	'2015-06-22',	23, 	0,	2)
	,('R3-2',	'2015-06-22',	23, 	0,	2)
	,('R3-3',	'2015-06-22',	23, 	1,	2)
	,('R3-4',	'2015-06-22',	23, 	1,	2)
	,('R3-5',	'2015-06-22',	23, 	1,	2)
	,('R4-1',	'2015-06-22',	24, 	1,	2)
	,('R4-2',	'2015-06-22',	24, 	1,	2)
	,('R4-3',	'2015-06-22',	24, 	1,	2)
	,('R4-4',	'2015-06-22',	24, 	1,	2)
	,('R4-5',	'2015-06-22',	24, 	1,	2)
	,('R5-1',	'2015-06-22',	25, 	1,	2)
	,('R5-2',	'2015-06-22',	25, 	1,	2)
	,('R5-3',	'2015-06-22',	25, 	1,	2)
	,('R5-4',	'2015-06-22',	25, 	1,	2)
	,('R5-5',	'2015-06-22',	25, 	1,	2)
	,('R6-1',	'2015-06-22',	26, 	1,	2)
	,('R6-2',	'2015-06-22',	26, 	1,	2)
	,('R6-3',	'2015-06-22',	26, 	1,	2)
	,('R6-4',	'2015-06-22',	26, 	1,	2)
	,('R6-5',	'2015-06-22',	26, 	1,	2)
	,('R7-1',	'2015-06-22',	27, 	1,	2)
	,('R7-2',	'2015-06-22',	27, 	1,	2)
	,('R7-3',	'2015-06-22',	27, 	1,	2)
	,('R7-4',	'2015-06-22',	27, 	1,	2)
	,('R7-5',	'2015-06-22',	27, 	1,	2)
	,('R8-1',	'2015-06-22',	28, 	1,	2)
	,('R8-2',	'2015-06-22',	28, 	1,	2)
	,('R8-3',	'2015-06-22',	28, 	1,	2)
	,('R8-4',	'2015-06-22',	28, 	1,	2)
	,('R8-5',	'2015-06-22',	28, 	1,	2)
	,('R9-1',	'2015-06-22',	29, 	1,	2)
	,('R9-2',	'2015-06-22',	29, 	1,	2)
	,('R9-3',	'2015-06-22',	29, 	1,	2)
	,('R9-4',	'2015-06-22',	29, 	1,	2)
	,('R9-5',	'2015-06-22',	29, 	0,	2)
	,('R10-1',	'2015-06-22',	30, 	0,	2)
	,('R10-2',	'2015-06-22',	30, 	1,	2)
	,('R10-3',	'2015-06-22',	30, 	1,	2)
	,('R10-4',	'2015-06-22',	30, 	1,	2)
	,('R10-5',	'2015-06-22',	30, 	1,	2)
	,('M1-1',	'2016-08-24',	31, 	1,	4)
	,('M1-2',	'2016-08-24',	31, 	1,	4)
	,('M1-3',	'2016-08-24',	31, 	1,	4)
	,('M1-4',	'2016-08-24',	31, 	1,	4)
	,('M1-5',	'2016-08-24',	31, 	1,	4)
	,('M2-1',	'2016-08-24',	32, 	1,	4)
	,('M2-2',	'2016-08-24',	32, 	1,	4)
	,('M2-3',	'2016-08-24',	32, 	0,	4)
	,('M2-4',	'2016-08-24',	32, 	0,	4)
	,('M2-5',	'2016-08-24',	32, 	0,	4)
	,('M3-1',	'2016-08-24',	33, 	0,	4)
	,('M3-2',	'2016-08-24',	33, 	0,	4)
	,('M3-3',	'2016-08-24',	33, 	0,	4)
	,('M3-4',	'2016-08-24',	33, 	1,	4)
	,('M3-5',	'2016-08-24',	33, 	1,	4)
	,('M4-1',	'2016-08-24',	34, 	1,	4)
	,('M4-2',	'2016-08-24',	34, 	1,	4)
	,('M4-3',	'2016-08-24',	34, 	1,	4)
	,('M4-4',	'2016-08-24',	34, 	1,	4)
	,('M4-5',	'2016-08-24',	34, 	1,	4)
	,('M5-1',	'2016-08-24',	35, 	1,	4)
	,('M5-2',	'2016-08-24',	35, 	1,	4)
	,('M5-3',	'2016-08-24',	35, 	1,	4)
	,('M5-4',	'2016-08-24',	35, 	1,	4)
	,('M5-5',	'2016-08-24',	35, 	1,	4)
	,('M6-1',	'2016-08-24',	36, 	1,	4)
	,('M6-2',	'2016-08-24',	36, 	1,	4)
	,('M6-3',	'2016-08-24',	36, 	1,	4)
	,('M6-4',	'2016-08-24',	36, 	1,	4)
	,('M6-5',	'2016-08-24',	36, 	1,	4)
	,('M7-1',	'2016-08-24',	37, 	1,	4)
	,('M7-2',	'2016-08-24',	37, 	1,	4)
	,('M7-3',	'2016-08-24',	37, 	1,	4)
	,('M7-4',	'2016-08-24',	37, 	1,	4)
	,('M7-5',	'2016-08-24',	37, 	1,	4)
	,('M8-1',	'2016-08-24',	38, 	1,	4)
	,('M8-2',	'2016-08-24',	38, 	1,	4)
	,('M8-3',	'2016-08-24',	38, 	1,	4)
	,('M8-4',	'2016-08-24',	38, 	1,	4)
	,('M8-5',	'2016-08-24',	38, 	1,	4)
	,('M9-1',	'2016-08-24',	39, 	1,	4)
	,('M9-2',	'2016-08-24',	39, 	1,	4)
	,('M9-3',	'2016-08-24',	39, 	1,	4)
	,('M9-4',	'2016-08-24',	39, 	1,	4)
	,('M9-5',	'2016-08-24',	39, 	1,	4)
	,('M10-1',	'2016-08-24',	40, 	1,	4)
	,('M10-2',	'2016-08-24',	40, 	1,	4)
	,('M10-3',	'2016-08-24',	40, 	0,	4)
	,('M10-4',	'2016-08-24',	40, 	0,	4)
	,('M10-5',	'2016-08-24',	40, 	1,	4)
	,('PN1-1',	'2017-06-23',	41, 	1,	3)
	,('PN1-2',	'2017-06-23',	41, 	1,	3)
	,('PN1-3',	'2017-06-23',	41, 	1,	3)
	,('PN1-4',	'2017-06-23',	41, 	1,	3)
	,('PN1-5',	'2017-06-23',	41, 	1,	3)
	,('PN2-1',	'2017-06-23',	42, 	1,	3)
	,('PN2-2',	'2017-06-23',	42, 	1,	3)
	,('PN2-3',	'2017-06-23',	42, 	1,	3)
	,('PN2-4',	'2017-06-23',	42, 	1,	3)
	,('PN2-5',	'2017-06-23',	42, 	1,	3)
	,('PN3-1',	'2017-06-23',	43, 	1,	3)
	,('PN3-2',	'2017-06-23',	43, 	1,	3)
	,('PN3-3',	'2017-06-23',	43, 	1,	3)
	,('PN3-4',	'2017-06-23',	43, 	1,	3)
	,('PN3-5',	'2017-06-23',	43, 	1,	3)
	,('PN4-1',	'2017-06-23',	44, 	1,	3)
	,('PN4-2',	'2017-06-23',	44, 	1,	3)
	,('PN4-3',	'2017-06-23',	44, 	1,	3)
	,('PN4-4',	'2017-06-23',	44, 	1,	3)
	,('PN4-5',	'2017-06-23',	44, 	1,	3)
	,('PN5-1',	'2017-06-23',	45, 	1,	3)
	,('PN5-2',	'2017-06-23',	45, 	1,	3)
	,('PN5-3',	'2017-06-23',	45, 	1,	3)
	,('PN5-4',	'2017-06-23',	45, 	1,	3)
	,('PN5-5',	'2017-06-23',	45, 	0,	3)
	,('PN6-1',	'2017-06-23',	46, 	0,	3)
	,('PN6-2',	'2017-06-23',	46, 	0,	3)
	,('PN6-3',	'2017-06-23',	46, 	0,	3)
	,('PN6-4',	'2017-06-23',	46, 	0,	3)
	,('PN6-5',	'2017-06-23',	46, 	0,	3)
	,('PN7-1',	'2017-06-23',	47, 	1,	3)
	,('PN7-2',	'2017-06-23',	47, 	1,	3)
	,('PN7-3',	'2017-06-23',	47, 	1,	3)
	,('PN7-4',	'2017-06-23',	47, 	1,	3)
	,('PN7-5',	'2017-06-23',	47, 	1,	3)
	,('PN8-1',	'2017-06-23',	48, 	1,	3)
	,('PN8-2',	'2017-06-23',	48, 	1,	3)
	,('PN8-3',	'2017-06-23',	48, 	1,	3)
	,('PN8-4',	'2017-06-23',	48, 	1,	3)
	,('PN8-5',	'2017-06-23',	48, 	1,	3)
	,('PN9-1',	'2017-06-23',	49, 	1,	3)
	,('PN9-2',	'2017-06-23',	49, 	1,	3)
	,('PN9-3',	'2017-06-23',	49, 	1,	3)
	,('PN9-4',	'2017-06-23',	49, 	1,	3)
	,('PN9-5',	'2017-06-23',	49, 	1,	3)
	,('PN10-1',	'2017-06-23',	50, 	1,	3)
	,('PN10-2',	'2017-06-23',	50, 	1,	3)
	,('PN10-3',	'2017-06-23',	50, 	1,	3)
	,('PN10-4',	'2017-06-23',	50, 	1,	3)
	,('PN10-5',	'2017-06-23',	50, 	1,	3)
	,('SB1-1',	'2016-04-28',	51, 	1,	5)
	,('SB1-2',	'2016-04-28',	51, 	1,	5)
	,('SB1-3',	'2016-04-28',	51, 	1,	5)
	,('SB1-4',	'2016-04-28',	51, 	1,	5)
	,('SB1-5',	'2016-04-28',	51, 	1,	5)
	,('SB2-1',	'2016-04-28',	52, 	1,	5)
	,('SB2-2',	'2016-04-28',	52, 	1,	5)
	,('SB2-3',	'2016-04-28',	52, 	1,	5)
	,('SB2-4',	'2016-04-28',	52, 	0,	5)
	,('SB2-5',	'2016-04-28',	52, 	0,	5)
	,('SB3-1',	'2016-04-28',	53, 	0,	5)
	,('SB3-2',	'2016-04-28',	53, 	1,	5)
	,('SB3-3',	'2016-04-28',	53, 	1,	5)
	,('SB3-4',	'2016-04-28',	53, 	1,	5)
	,('SB3-5',	'2016-04-28',	53, 	1,	5)
	,('SB4-1',	'2016-04-28',	54, 	1,	5)
	,('SB4-2',	'2016-04-28',	54, 	1,	5)
	,('SB4-3',	'2016-04-28',	54, 	1,	5)
	,('SB4-4',	'2016-04-28',	54, 	1,	5)
	,('SB4-5',	'2016-04-28',	54, 	1,	5)
	,('SB5-1',	'2016-04-28',	55, 	1,	5)
	,('SB5-2',	'2016-04-28',	55, 	1,	5)
	,('SB5-3',	'2016-04-28',	55, 	1,	5)
	,('SB5-4',	'2016-04-28',	55, 	1,	5)
	,('SB5-5',	'2016-04-28',	55, 	1,	5)
	,('SB6-1',	'2016-04-28',	56, 	1,	5)
	,('SB6-2',	'2016-04-28',	56, 	1,	5)
	,('SB6-3',	'2016-04-28',	56, 	1,	5)
	,('SB6-4',	'2016-04-28',	56, 	1,	5)
	,('SB6-5',	'2016-04-28',	56, 	1,	5)
	,('SB7-1',	'2016-04-28',	57, 	1,	5)
	,('SB7-2',	'2016-04-28',	57, 	1,	5)
	,('SB7-3',	'2016-04-28',	57, 	1,	5)
	,('SB7-4',	'2016-04-28',	57, 	1,	5)
	,('SB7-5',	'2016-04-28',	57, 	1,	5)
	,('SB8-1',	'2016-04-28',	58, 	1,	5)
	,('SB8-2',	'2016-04-28',	58, 	1,	5)
	,('SB8-3',	'2016-04-28',	58, 	1,	5)
	,('SB8-4',	'2016-04-28',	58, 	1,	5)
	,('SB8-5',	'2016-04-28',	58, 	0,	5)
	,('SB9-1',	'2016-04-28',	59, 	0,	5)
	,('SB9-2',	'2016-04-28',	59, 	0,	5)
	,('SB9-3',	'2016-04-28',	59, 	1,	5)
	,('SB9-4',	'2016-04-28',	59, 	1,	5)
	,('SB9-5',	'2016-04-28',	59, 	1,	5)
	,('SB10-1',	'2016-04-28',	60, 	1,	5)
	,('SB10-2',	'2016-04-28',	60, 	1,	5)
	,('SB10-3',	'2016-04-28',	60, 	1,	5)
	,('SB10-4',	'2016-04-28',	60, 	1,	5)
	,('SB10-5',	'2016-04-28',	60, 	1,	5)
	,('SB11-1',	'2016-04-28',	61, 	1,	5)
	,('SB11-2',	'2016-04-28',	61, 	1,	5)
	,('SB11-3',	'2016-04-28',	61, 	1,	5)
	,('SB11-4',	'2016-04-28',	61, 	1,	5)
	,('SB11-5',	'2016-04-28',	61, 	1,	5)
	,('SB12-1',	'2016-04-28',	62, 	1,	5)
	,('SB12-2',	'2016-04-28',	62, 	1,	5)
	,('SB12-3',	'2016-04-28',	62, 	1,	5)
	,('SB12-4',	'2016-04-28',	62, 	1,	5)
	,('SB12-5',	'2016-04-28',	62, 	0,	5)
	,('SB13-1',	'2016-04-28',	63, 	0,	5)
	,('SB13-2',	'2016-04-28',	63, 	1,	5)
	,('SB13-3',	'2016-04-28',	63, 	1,	5)
	,('SB13-4',	'2016-04-28',	63, 	1,	5)
	,('SB13-5',	'2016-04-28',	63, 	1,	5)
	,('SB14-1',	'2016-04-28',	64, 	1,	5)
	,('SB14-2',	'2016-04-28',	64, 	1,	5)
	,('SB14-3',	'2016-04-28',	64, 	1,	5)
	,('SB14-4',	'2016-04-28',	64, 	1,	5)
	,('SB14-5',	'2016-04-28',	64, 	1,	5)
	,('SB15-1',	'2016-04-28',	65, 	1,	5)
	,('SB15-2',	'2016-04-28',	65, 	1,	5)
	,('SB15-3',	'2016-04-28',	65, 	1,	5)
	,('SB15-4',	'2016-04-28',	65, 	1,	5)
	,('SB15-5',	'2016-04-28',	65, 	1,	5)
	,('SB16-1',	'2016-04-28',	66, 	1,	5)
	,('SB16-2',	'2016-04-28',	66, 	1,	5)
	,('SB16-3',	'2016-04-28',	66, 	1,	5)
	,('SB16-4',	'2016-04-28',	66, 	1,	5)
	,('SB16-5',	'2016-04-28',	66, 	1,	5)
	,('SB17-1',	'2016-04-28',	67, 	1,	5)
	,('SB17-2',	'2016-04-28',	67, 	1,	5)
	,('SB17-3',	'2016-04-28',	67, 	1,	5)
	,('SB17-4',	'2016-04-28',	67, 	1,	5)
	,('SB17-5',	'2016-04-28',	67, 	1,	5)
	,('SB18-1',	'2016-04-28',	68, 	1,	5)
	,('SB18-2',	'2016-04-28',	68, 	1,	5)
	,('SB18-3',	'2016-04-28',	68, 	1,	5)
	,('SB18-4',	'2016-04-28',	68, 	1,	5)
	,('SB18-5',	'2016-04-28',	68, 	1,	5)
	,('SB19-1',	'2016-04-28',	69, 	0,	5)
	,('SB19-2',	'2016-04-28',	69, 	0,	5)
	,('SB19-3',	'2016-04-28',	69, 	0,	5)
	,('SB19-4',	'2016-04-28',	69, 	1,	5)
	,('SB19-5',	'2016-04-28',	69, 	1,	5)
	,('SB20-1',	'2016-04-28',	70, 	1,	5)
	,('SB20-2',	'2016-04-28',	70, 	1,	5)
	,('SB20-3',	'2016-04-28',	70, 	1,	5)
	,('SB20-4',	'2016-04-28',	70, 	1,	5)
	,('SB20-5',	'2016-04-28',	70, 	1,	5)
	,('PG1-1',	'2016-06-28',	71, 	1,	6)
	,('PG1-2',	'2016-06-28',	71, 	1,	6)
	,('PG1-3',	'2016-06-28',	71, 	1,	6)
	,('PG1-4',	'2016-06-28',	71, 	1,	6)
	,('PG1-5',	'2016-06-28',	71, 	1,	6)
	,('PG2-1',	'2016-06-28',	72, 	1,	6)
	,('PG2-2',	'2016-06-28',	72, 	1,	6)
	,('PG2-3',	'2016-06-28',	72, 	1,	6)
	,('PG2-4',	'2016-06-28',	72, 	1,	6)
	,('PG2-5',	'2016-06-28',	72, 	1,	6)
	,('PG3-1',	'2016-06-28',	73, 	1,	6)
	,('PG3-2',	'2016-06-28',	73, 	1,	6)
	,('PG3-3',	'2016-06-28',	73, 	1,	6)
	,('PG3-4',	'2016-06-28',	73, 	1,	6)
	,('PG3-5',	'2016-06-28',	73, 	1,	6)
	,('PG4-1',	'2016-06-28',	74, 	1,	6)
	,('PG4-2',	'2016-06-28',	74, 	1,	6)
	,('PG4-3',	'2016-06-28',	74, 	1,	6)
	,('PG4-4',	'2016-06-28',	74, 	1,	6)
	,('PG4-5',	'2016-06-28',	74, 	1,	6)
	,('PG5-1',	'2016-06-28',	75, 	1,	6)
	,('PG5-2',	'2016-06-28',	75, 	0,	6)
	,('PG5-3',	'2016-06-28',	75, 	0,	6)
	,('PG5-4',	'2016-06-28',	75, 	0,	6)
	,('PG5-5',	'2016-06-28',	75, 	0,	6)
	,('PG6-1',	'2016-06-28',	76, 	1,	6)
	,('PG6-2',	'2016-06-28',	76, 	1,	6)
	,('PG6-3',	'2016-06-28',	76, 	1,	6)
	,('PG6-4',	'2016-06-28',	76, 	1,	6)
	,('PG6-5',	'2016-06-28',	76, 	1,	6)
	,('PG7-1',	'2016-06-28',	77, 	1,	6)
	,('PG7-2',	'2016-06-28',	77, 	1,	6)
	,('PG7-3',	'2016-06-28',	77, 	1,	6)
	,('PG7-4',	'2016-06-28',	77, 	1,	6)
	,('PG7-5',	'2016-06-28',	77, 	1,	6)
	,('PG8-1',	'2016-06-28',	78, 	1,	6)
	,('PG8-2',	'2016-06-28',	78, 	1,	6)
	,('PG8-3',	'2016-06-28',	78, 	1,	6)
	,('PG8-4',	'2016-06-28',	78, 	1,	6)
	,('PG8-5',	'2016-06-28',	78, 	1,	6)
	,('PG9-1',	'2016-06-28',	79, 	1,	6)
	,('PG9-2',	'2016-06-28',	79, 	1,	6)
	,('PG9-3',	'2016-06-28',	79, 	1,	6)
	,('PG9-4',	'2016-06-28',	79, 	1,	6)
	,('PG9-5',	'2016-06-28',	79, 	1,	6)
	,('PG10-1',	'2016-06-28',	80, 	1,	6)
	,('PG10-2',	'2016-06-28',	80, 	1,	6)
	,('PG10-3',	'2016-06-28',	80, 	1,	6)
	,('PG10-4',	'2016-06-28',	80, 	1,	6)
	,('PG10-5',	'2016-06-28',	80, 	1,	6)
	,('C21-1',	'2019-04-23',	81, 	1,	1)
	,('C21-2',	'2019-04-23',	81, 	1,	1)
	,('C21-3',	'2019-04-23',	81, 	1,	1)
	,('C21-4',	'2019-04-23',	81, 	1,	1)
	,('C21-5',	'2019-04-23',	81, 	1,	1)
	,('C22-1',	'2019-04-23',	82, 	1,	1)
	,('C22-2',	'2019-04-23',	82, 	1,	1)
	,('C22-3',	'2019-04-23',	82, 	1,	1)
	,('C22-4',	'2019-04-23',	82, 	1,	1)
	,('C22-5',	'2019-04-23',	82, 	1,	1)
	,('C23-1',	'2019-04-23',	83, 	1,	1)
	,('C23-2',	'2019-04-23',	83, 	1,	1)
	,('C23-3',	'2019-04-23',	83, 	1,	1)
	,('C23-4',	'2019-04-23',	83, 	1,	1)
	,('C23-5',	'2019-04-23',	83, 	1,	1)
	,('C24-1',	'2019-04-23',	84, 	1,	1)
	,('C24-2',	'2019-04-23',	84, 	0,	1)
	,('C24-3',	'2019-04-23',	84, 	0,	1)
	,('C24-4',	'2019-04-23',	84, 	0,	1)
	,('C24-5',	'2019-04-23',	84, 	0,	1)
	,('C25-1',	'2019-04-23',	85, 	0,	1)
	,('C25-2',	'2019-04-23',	85, 	0,	1)
	,('C25-3',	'2019-04-23',	85, 	0,	1)
	,('C25-4',	'2019-04-23',	85, 	0,	1)
	,('C25-5',	'2019-04-23',	85, 	0,	1)
	,('C26-1',	'2019-04-23',	86, 	0,	1)
	,('C26-2',	'2019-04-23',	86, 	0,	1)
	,('C26-3',	'2019-04-23',	86, 	1,	1)
	,('C26-4',	'2019-04-23',	86, 	1,	1)
	,('C26-5',	'2019-04-23',	86, 	1,	1)
	,('C27-1',	'2019-04-23',	87, 	1,	1)
	,('C27-2',	'2019-04-23',	87, 	1,	1)
	,('C27-3',	'2019-04-23',	87, 	1,	1)
	,('C27-4',	'2019-04-23',	87, 	1,	1)
	,('C27-5',	'2019-04-23',	87, 	1,	1)
	,('C28-1',	'2019-04-23',	88, 	1,	1)
	,('C28-2',	'2019-04-23',	88, 	1,	1)
	,('C28-3',	'2019-04-23',	88, 	1,	1)
	,('C28-4',	'2019-04-23',	88, 	1,	1)
	,('C28-5',	'2019-04-23',	88, 	1,	1)
	,('C29-1',	'2019-04-23',	89, 	1,	1)
	,('C29-2',	'2019-04-23',	89, 	1,	1)
	,('C29-3',	'2019-04-23',	89, 	1,	1)
	,('C29-4',	'2019-04-23',	89, 	0,	1)
	,('C29-5',	'2019-04-23',	89, 	0,	1)
	,('C30-1',	'2019-04-23',	90, 	0,	1)
	,('C30-2',	'2019-04-23',	90, 	0,	1)
	,('C30-3',	'2019-04-23',	90, 	0,	1)
	,('C30-4',	'2019-04-23',	90, 	0,	1)
	,('C30-5',	'2019-04-23',	90, 	0,	1)
	,('C31-1',	'2019-04-23',	91, 	0,	1)
	,('C31-2',	'2019-04-23',	91, 	0,	1)
	,('C31-3',	'2019-04-23',	91, 	0,	1)
	,('C31-4',	'2019-04-23',	91, 	1,	1)
	,('C31-5',	'2019-04-23',	91, 	1,	1)
	,('C32-1',	'2019-04-23',	92, 	1,	1)
	,('C32-2',	'2019-04-23',	92, 	1,	1)
	,('C32-3',	'2019-04-23',	92, 	1,	1)
	,('C32-4',	'2019-04-23',	92, 	1,	1)
	,('C32-5',	'2019-04-23',	92, 	1,	1)
	,('C33-1',	'2019-04-23',	93, 	1,	1)
	,('C33-2',	'2019-04-23',	93, 	1,	1)
	,('C33-3',	'2019-04-23',	93, 	1,	1)
	,('C33-4',	'2019-04-23',	93, 	1,	1)
	,('C33-5',	'2019-04-23',	93, 	1,	1)
	,('C34-1',	'2019-04-23',	94, 	1,	1)
	,('C34-2',	'2019-04-23',	94, 	1,	1)
	,('C34-3',	'2019-04-23',	94, 	1,	1)
	,('C34-4',	'2019-04-23',	94, 	1,	1)
	,('C34-5',	'2019-04-23',	94, 	1,	1)
	,('C35-1',	'2019-04-23',	95, 	1,	1)
	,('C35-2',	'2019-04-23',	95, 	1,	1)
	,('C35-3',	'2019-04-23',	95, 	1,	1)
	,('C35-4',	'2019-04-23',	95, 	1,	1)
	,('C35-5',	'2019-04-23',	95, 	1,	1)
	,('C36-1',	'2019-04-23',	96, 	1,	1)
	,('C36-2',	'2019-04-23',	96, 	1,	1)
	,('C36-3',	'2019-04-23',	96, 	1,	1)
	,('C36-4',	'2019-04-23',	96, 	1,	1)
	,('C36-5',	'2019-04-23',	96, 	1,	1)
	,('C37-1',	'2019-04-23',	97, 	1,	1)
	,('C37-2',	'2019-04-23',	97, 	1,	1)
	,('C37-3',	'2019-04-23',	97, 	1,	1)
	,('C37-4',	'2019-04-23',	97, 	1,	1)
	,('C37-5',	'2019-04-23',	97, 	1,	1)
	,('C38-1',	'2019-04-23',	98, 	0,	1)
	,('C38-2',	'2019-04-23',	98, 	0,	1)
	,('C38-3',	'2019-04-23',	98, 	0,	1)
	,('C38-4',	'2019-04-23',	98, 	0,	1)
	,('C38-5',	'2019-04-23',	98, 	0,	1)
	,('C39-1',	'2019-04-23',	99, 	0,	1)
	,('C39-2',	'2019-04-23',	99, 	0,	1)
	,('C39-3',	'2019-04-23',	99, 	0,	1)
	,('C39-4',	'2019-04-23',	99, 	0,	1)
	,('C39-5',	'2019-04-23',	99, 	0,	1)
	,('C40-1',	'2019-04-23',	100,	0,	1)
	,('C40-2',	'2019-04-23',	100,	0,	1)
	,('C40-3',	'2019-04-23',	100,	0,	1)
	,('C40-4',	'2019-04-23',	100,	1,	1)
	,('C40-5',	'2019-04-23',	100,	1,	1)
	,('R11-1',	'2019-06-30',	101,	1,	2)
	,('R11-2',	'2019-06-30',	101,	1,	2)
	,('R11-3',	'2019-06-30',	101,	1,	2)
	,('R11-4',	'2019-06-30',	101,	1,	2)
	,('R11-5',	'2019-06-30',	101,	1,	2)
	,('R12-1',	'2019-06-30',	102,	1,	2)
	,('R12-2',	'2019-06-30',	102,	1,	2)
	,('R12-3',	'2019-06-30',	102,	1,	2)
	,('R12-4',	'2019-06-30',	102,	1,	2)
	,('R12-5',	'2019-06-30',	102,	1,	2)
	,('R13-1',	'2019-06-30',	103,	1,	2)
	,('R13-2',	'2019-06-30',	103,	1,	2)
	,('R13-3',	'2019-06-30',	103,	1,	2)
	,('R13-4',	'2019-06-30',	103,	1,	2)
	,('R13-5',	'2019-06-30',	104,	0,	2)
	,('R14-1',	'2019-06-30',	104,	0,	2)
	,('R14-2',	'2019-06-30',	104,	0,	2)
	,('R14-3',	'2019-06-30',	104,	0,	2)
	,('R14-4',	'2019-06-30',	104,	0,	2)
	,('R14-5',	'2019-06-30',	104,	0,	2)
	,('R15-1',	'2019-06-30',	105,	0,	2)
	,('R15-2',	'2019-06-30',	105,	0,	2)
	,('R15-3',	'2019-06-30',	105,	0,	2)
	,('R15-4',	'2019-06-30',	105,	0,	2)
	,('R15-5',	'2019-06-30',	105,	0,	2)
	,('R16-1',	'2019-06-30',	106,	0,	2)
	,('R16-2',	'2019-06-30',	106,	0,	2)
	,('R16-3',	'2019-06-30',	106,	1,	2)
	,('R16-4',	'2019-06-30',	106,	1,	2)
	,('R16-5',	'2019-06-30',	106,	1,	2)
	,('R17-1',	'2019-06-30',	107,	1,	2)
	,('R17-2',	'2019-06-30',	107,	1,	2)
	,('R17-3',	'2019-06-30',	107,	1,	2)
	,('R17-4',	'2019-06-30',	107,	1,	2)
	,('R17-5',	'2019-06-30',	107,	1,	2)
	,('R18-1',	'2019-06-30',	108,	1,	2)
	,('R18-2',	'2019-06-30',	108,	1,	2)
	,('R18-3',	'2019-06-30',	108,	1,	2)
	,('R18-4',	'2019-06-30',	108,	1,	2)
	,('R18-5',	'2019-06-30',	108,	1,	2)
	,('R19-1',	'2019-06-30',	109,	1,	2)
	,('R19-2',	'2019-06-30',	109,	1,	2)
	,('R19-3',	'2019-06-30',	109,	1,	2)
	,('R19-4',	'2019-06-30',	109,	1,	2)
	,('R19-5',	'2019-06-30',	109,	1,	2)
	,('R20-1',	'2019-06-30',	110,	1,	2)
	,('R20-2',	'2019-06-30',	110,	1,	2)
	,('R20-3',	'2019-06-30',	110,	1,	2)
	,('R20-4',	'2019-06-30',	110,	1,	2)
	,('R20-5',	'2019-06-30',	111,	1,	2)
	,('M11-1',	'2020-08-29',	111,	1,	4)
	,('M11-2',	'2020-08-29',	111,	1,	4)
	,('M11-3',	'2020-08-29',	111,	1,	4)
	,('M11-4',	'2020-08-29',	111,	1,	4)
	,('M11-5',	'2020-08-29',	111,	1,	4)
	,('M12-1',	'2020-08-29',	112,	1,	4)
	,('M12-2',	'2020-08-29',	112,	1,	4)
	,('M12-3',	'2020-08-29',	112,	1,	4)
	,('M12-4',	'2020-08-29',	112,	1,	4)
	,('M12-5',	'2020-08-29',	112,	1,	4)
	,('M13-1',	'2020-08-29',	113,	1,	4)
	,('M13-2',	'2020-08-29',	113,	1,	4)
	,('M13-3',	'2020-08-29',	113,	1,	4)
	,('M13-4',	'2020-08-29',	113,	1,	4)
	,('M13-5',	'2020-08-29',	113,	1,	4)
	,('M14-1',	'2020-08-29',	114,	0,	4)
	,('M14-2',	'2020-08-29',	114,	0,	4)
	,('M14-3',	'2020-08-29',	114,	0,	4)
	,('M14-4',	'2020-08-29',	114,	0,	4)
	,('M14-5',	'2020-08-29',	114,	0,	4)
	,('M15-1',	'2020-08-29',	115,	0,	4)
	,('M15-2',	'2020-08-29',	115,	0,	4)
	,('M15-3',	'2020-08-29',	115,	0,	4)
	,('M15-4',	'2020-08-29',	115,	0,	4)
	,('M15-5',	'2020-08-29',	115,	0,	4)
	,('M16-1',	'2020-08-29',	115,	0,	4)
	,('M16-2',	'2020-08-29',	116,	0,	4)
	,('M16-3',	'2020-08-29',	116,	0,	4)
	,('M16-4',	'2020-08-29',	116,	0,	4)
	,('M16-5',	'2020-08-29',	116,	0,	4)
	,('M17-1',	'2020-08-29',	117,	1,	4)
	,('M17-2',	'2020-08-29',	117,	1,	4)
	,('M17-3',	'2020-08-29',	117,	1,	4)
	,('M17-4',	'2020-08-29',	117,	1,	4)
	,('M17-5',	'2020-08-29',	117,	1,	4)
	,('M18-1',	'2020-08-29',	118,	1,	4)
	,('M18-2',	'2020-08-29',	118,	1,	4)
	,('M18-3',	'2020-08-29',	118,	1,	4)
	,('M18-4',	'2020-08-29',	118,	1,	4)
	,('M18-5',	'2020-08-29',	118,	1,	4)
	,('M19-1',	'2020-08-29',	119,	1,	4)
	,('M19-2',	'2020-08-29',	119,	1,	4)
	,('M19-3',	'2020-08-29',	119,	1,	4)
	,('M19-4',	'2020-08-29',	119,	1,	4)
	,('M19-5',	'2020-08-29',	119,	1,	4)
	,('M20-1',	'2020-08-29',	120,	1,	4)
	,('M20-2',	'2020-08-29',	120,	1,	4)
	,('M20-3',	'2020-08-29',	120,	1,	4)
	,('M20-4',	'2020-08-29',	120,	1,	4)
	,('M20-5',	'2020-08-29',	120,	1,	4)
	,('PN11-1',	'2021-06-15',	121,	1,	3)
	,('PN11-2',	'2021-06-15',	121,	1,	3)
	,('PN11-3',	'2021-06-15',	121,	1,	3)
	,('PN11-4',	'2021-06-15',	121,	1,	3)
	,('PN11-5',	'2021-06-15',	121,	1,	3)
	,('PN12-1',	'2021-06-15',	122,	1,	3)
	,('PN12-2',	'2021-06-15',	122,	1,	3)
	,('PN12-3',	'2021-06-15',	122,	1,	3)
	,('PN12-4',	'2021-06-15',	122,	1,	3)
	,('PN12-5',	'2021-06-15',	122,	1,	3)
	,('PN13-1',	'2021-06-15',	123,	1,	3)
	,('PN13-2',	'2021-06-15',	123,	1,	3)
	,('PN13-3',	'2021-06-15',	123,	1,	3)
	,('PN13-4',	'2021-06-15',	123,	1,	3)
	,('PN13-5',	'2021-06-15',	123,	1,	3)
	,('PN14-1',	'2021-06-15',	124,	1,	3)
	,('PN14-2',	'2021-06-15',	124,	1,	3)
	,('PN14-3',	'2021-06-15',	124,	1,	3)
	,('PN14-4',	'2021-06-15',	124,	1,	3)
	,('PN14-5',	'2021-06-15',	124,	1,	3)
	,('PN15-1',	'2021-06-15',	125,	1,	3)
	,('PN15-2',	'2021-06-15',	125,	1,	3)
	,('PN15-3',	'2021-06-15',	125,	1,	3)
	,('PN15-4',	'2021-06-15',	125,	1,	3)
	,('PN15-5',	'2021-06-15',	125,	1,	3)
	,('PN16-1',	'2021-06-15',	126,	1,	3)
	,('PN16-2',	'2021-06-15',	126,	0,	3)
	,('PN16-3',	'2021-06-15',	126,	0,	3)
	,('PN16-4',	'2021-06-15',	126,	0,	3)
	,('PN16-5',	'2021-06-15',	126,	0,	3)
	,('PN17-1',	'2021-06-15',	127,	0,	3)
	,('PN17-2',	'2021-06-15',	127,	0,	3)
	,('PN17-3',	'2021-06-15',	127,	0,	3)
	,('PN17-4',	'2021-06-15',	127,	0,	3)
	,('PN17-5',	'2021-06-15',	127,	0,	3)
	,('PN18-1',	'2021-06-15',	128,	0,	3)
	,('PN18-2',	'2021-06-15',	128,	0,	3)
	,('PN18-3',	'2021-06-15',	128,	0,	3)
	,('PN18-4',	'2021-06-15',	128,	0,	3)
	,('PN18-5',	'2021-06-15',	128,	0,	3)



-- Sales Tables

INSERT INTO customers
	VALUES
	('Claire',		'Gute',			'Claire@gmail.com',			'Ap #211-6546 Nec Rd.',					'Henderson',		'Kentucky',			'42420')
	,('Darrin',		'Van',			'Darrin@yahoo.net',			'P.O. Box 240, 6674 Ultrices. St.',		'Los Angeles',		'California',		'90036')
	,('Sean',		'ODonnell',		'Sean@gmail.com',			'344-9847 Convallis Rd.',				'Fort Lauderdale',	'Florida',			'33311')
	,('Brosina',	'Hoffman',		'Brosina@yahoo.net',		'406-3292 Varius Rd.',					'Los Angeles',		'California',		'90032')
	,('Andrew',		'Allen',		'Andrew@gmail.com',			'377-787 Duis Avenue',					'Concord',			'North Carolina',	'28027')
	,('Irene',		'Maddox',		'Irene@yahoo.net',			'Ap#230-9624 Enim Ave',					'Seattle',			'Washington',		'98103')
	,('Harold',		'Pawlan',		'Harold@gmail.com',			'142-917 Erat Av',						'Fort Worth',		'Texas',			'76106')
	,('Pete',		'Kriz',			'Pete@yahoo.net',			'166-9379 Odio. Rd.',					'Madison',			'Wisconsin',		'53711')
	,('Alejandro',	'Grove',		'Alejandro@gmail.com',		'6440 Ipsum Road',						'West Jordan',		'Utah',				'84084')
	,('Zuschuss',	'Donatelli',	'Zuschuss@yahoo.net',		'Ap #918-9972 Iaculis Rd.',				'San Francisco',	'California',		'94109')
	,('Ken',		'Black',		'Ken@gmail.com',			'3201 Donec Ave',						'Fremont',			'Nebraska',			'68025')
	,('Sandra',		'Flanagan',		'Sandra@yahoo.net',			'Ap #227-4271 Congue, Av.',				'Philadelphia',		'Pennsylvania',		'19140')
	,('Emily',		'Burns',		'Emily@gmail.com',			'2546 Rhoncus St.',						'Orem',				'Utah',				'84057')
	,('Eric',		'Hoffmann',		'Eric@yahoo.net',			'Ap #245-6950 Non, Street',				'Los Angeles',		'California',		'90049')
	,('Tracy',		'Blumstein',	'Tracy@gmail.com',			'431-1617 Fermentum Street',			'Philadelphia',		'Pennsylvania',		'19140')
	,('Matt',		'Abelman',		'Matt@yahoo.net',			'Ap #788-1232 Erat, Street',			'Houston',			'Texas',			'77095')
	,('Gene',		'Hale',			'Gene@gmail.com',			'3201 Donec Ave',						'Richardson',		'Texas',			'75080')
	,('Steve',		'Nguyen',		'Steve@yahoo.net',			'Ap #227-4271 Congue, Av',				'Houston',			'Texas',			'77041')
	,('Linda',		'Cazamias',		'Linda@gmail.com',			'Ap #433-7520 Malesuada Ave',			'Naperville',		'Illinois',			'60540')
	,('Ruben',		'Ausman',		'Ruben@yahoo.net',			'814-8447 Tincidunt Ave',				'Los Angeles',		'California',		'90049')
	,('Erin',		'Smith',		'Erin@gmail.com',			'P.O. Box 675, 7896 Velit. Ave',		'Melbourne',		'Florida',			'32935')
	,('Odella',		'Nelson',		'Odella@yahoo.net',			'948-1670 Purus. Avenue',				'Eagan',			'Minnesota',		'55122')
	,('Patrick',	'ODonnell',		'Patrick@gmail.com',		'431-1617 Fermentum Street',			'Westland',			'Michigan',			'48185')

INSERT INTO shippers
	VALUES
	('FEDEX',	'7862123654')
	,('UPS',		'3026345905')
	,('USPS',	'3052299011')


INSERT INTO orders
	VALUES
	(547	,8	,2	,1	,'2020-01-03')
	,(248	,11	,2	,1	,'2020-01-04')
	,(171	,18	,2	,1	,'2020-01-05')
	,(558	,7	,2	,1	,'2020-01-06')
	,(245	,14	,2	,1	,'2020-01-10')
	,(592	,14	,2	,1	,'2020-01-10')
	,(74		,10	,1	,1	,'2020-01-11')
	,(79		,4	,3	,1	,'2020-01-12')
	,(174	,15	,1	,1	,'2020-01-14')
	,(535	,10	,3	,1	,'2020-01-14')
	,(612	,3	,1	,1	,'2020-01-14')
	,(127	,12	,2	,1	,'2020-01-21')
	,(166	,1	,1	,1	,'2020-01-21')
	,(589	,9	,2	,1	,'2020-01-22')
	,(34		,8	,3	,1	,'2020-01-23')
	,(393	,20	,3	,1	,'2020-01-23')
	,(470	,3	,3	,1	,'2020-01-26')
	,(303	,9	,1	,1	,'2020-01-28')
	,(359	,20	,3	,1	,'2020-01-30')
	,(177	,21	,2	,1	,'2020-02-04')
	,(192	,23	,1	,1	,'2020-02-08')
	,(169	,1	,3	,1	,'2020-02-09')
	,(430	,11	,1	,1	,'2020-02-09')
	,(387	,18	,3	,1	,'2020-02-10')
	,(32		,8	,3	,1	,'2020-02-12')	  
	,(122	,5	,3	,1	,'2020-02-13')
	,(459	,19	,1	,1	,'2020-02-17')
	,(284	,1	,2	,1	,'2020-02-20')
	,(233	,20	,2	,1	,'2020-02-21')
	,(423	,15	,1	,1	,'2020-02-23')
	,(57		,21	,1	,1	,'2020-02-24')	  
	,(395	,18	,3	,1	,'2020-02-27')
	,(458	,7	,2	,1	,'2020-02-28')
	,(142	,15	,1	,1	,'2020-03-08')
	,(168	,5	,1	,1	,'2020-03-09')
	,(351	,22	,3	,1	,'2020-03-10')
	,(29		,4	,2	,1	,'2020-03-11')	  
	,(209	,18	,1	,1	,'2020-03-14')
	,(172	,4	,1	,1	,'2020-03-16')
	,(76		,8	,2	,1	,'2020-03-18')	  
	,(434	,15	,3	,1	,'2020-03-19')
	,(48		,2	,2	,1	,'2020-03-20')	  
	,(38		,19	,1	,1	,'2020-03-26')	  
	,(462	,14	,1	,1	,'2020-03-29')
	,(316	,8	,2	,1	,'2020-04-01')
	,(189	,1	,2	,1	,'2020-04-02')
	,(15		,19	,1	,1	,'2020-04-06')	  
	,(247	,21	,3	,1	,'2020-04-08')
	,(326	,4	,1	,1	,'2020-04-11')
	,(322	,17	,2	,1	,'2020-04-13')
	,(37		,16	,3	,1	,'2020-04-14')	  
	,(84		,17	,3	,1	,'2020-04-15')	  
	,(162	,6	,2	,1	,'2020-04-17')
	,(44		,4	,3	,1	,'2020-04-18')	  
	,(238	,16	,3	,1	,'2020-04-18')
	,(272	,18	,1	,1	,'2020-04-18')
	,(133	,23	,2	,1	,'2020-04-20')
	,(230	,19	,1	,1	,'2020-04-21')
	,(509	,23	,3	,1	,'2020-04-22')
	,(472	,23	,3	,1	,'2020-04-23')
	,(588	,18	,3	,1	,'2020-04-25')
	,(228	,15	,3	,1	,'2020-04-26')
	,(267	,6	,2	,1	,'2020-04-26')
	,(380	,4	,2	,1	,'2020-04-30')
	,(138	,14	,1	,1	,'2020-05-03')
	,(180	,21	,2	,1	,'2020-05-05')
	,(474	,18	,3	,1	,'2020-05-10')
	,(583	,19	,1	,1	,'2020-05-11')
	,(204	,19	,3	,1	,'2020-05-21')
	,(349	,2	,1	,1	,'2020-05-23')
	,(211	,22	,1	,1	,'2020-05-24')
	,(377	,7	,2	,1	,'2020-05-26')
	,(371	,12	,3	,1	,'2020-05-29')
	,(534	,5	,2	,1	,'2020-06-05')
	,(619	,7	,2	,1	,'2020-06-08')
	,(356	,11	,3	,1	,'2020-06-09')
	,(433	,4	,3	,1	,'2020-06-10')
	,(281	,16	,1	,1	,'2020-06-11')
	,(244	,12	,2	,1	,'2020-06-12')
	,(219	,21	,2	,1	,'2020-06-16')
	,(527	,13	,2	,1	,'2020-06-16')
	,(453	,7	,1	,1	,'2020-06-17')
	,(576	,11	,1	,1	,'2020-06-18')
	,(215	,18	,2	,1	,'2020-06-19')
	,(288	,5	,3	,1	,'2020-06-22')
	,(199	,22	,1	,1	,'2020-06-23')
	,(270	,9	,1	,1	,'2020-06-23')
	,(278	,7	,1	,1	,'2020-06-26')
	,(526	,6	,3	,1	,'2020-06-26')
	,(120	,17	,3	,1	,'2020-06-27')
	,(88		,16	,3	,1	,'2020-06-29')	  
	,(317	,12	,1	,1	,'2020-07-01')
	,(466	,11	,2	,1	,'2020-07-02')
	,(82		,22	,1	,1	,'2020-07-04')	  
	,(584	,12	,2	,1	,'2020-07-06')
	,(609	,8	,1	,1	,'2020-07-09')
	,(103	,20	,2	,1	,'2020-07-12')
	,(23		,9	,1	,1	,'2020-07-13')	  
	,(135	,13	,2	,1	,'2020-07-15')
	,(78		,7	,3	,1	,'2020-07-18')	  
	,(179	,7	,2	,1	,'2020-07-20')
	,(392	,6	,3	,1	,'2020-07-20')
	,(181	,7	,1	,1	,'2020-07-23')
	,(355	,7	,1	,1	,'2020-08-02')
	,(275	,20	,1	,1	,'2020-08-09')
	,(465	,11	,3	,1	,'2020-08-10')
	,(47		,22	,3	,1	,'2020-08-12')	  
	,(110	,16	,3	,1	,'2020-08-14')
	,(480	,3	,2	,1	,'2020-08-17')
	,(259	,20	,3	,1	,'2020-08-20')
	,(73		,4	,2	,1	,'2020-08-22')	  
	,(99		,14	,2	,1	,'2020-08-24')	  
	,(144	,5	,3	,1	,'2020-08-24')
	,(320	,15	,1	,1	,'2020-08-28')
	,(202	,2	,1	,1	,'2020-08-30')
	,(502	,17	,2	,1	,'2020-08-30')
	,(605	,4	,1	,1	,'2020-09-02')
	,(461	,21	,1	,1	,'2020-09-06')
	,(41		,8	,3	,1	,'2020-09-09')	  
	,(425	,13	,2	,1	,'2020-09-11')
	,(253	,17	,2	,1	,'2020-09-12')
	,(450	,18	,3	,1	,'2020-09-14')
	,(11		,22	,2	,1	,'2020-09-20')	  
	,(294	,17	,2	,1	,'2020-09-24')
	,(600	,19	,3	,1	,'2020-09-30')
	,(98		,17	,2	,1	,'2020-10-01')	  
	,(173	,17	,2	,1	,'2020-10-03')
	,(69		,13	,3	,1	,'2020-10-04')	  
	,(343	,10	,2	,1	,'2020-10-04')
	,(170	,18	,2	,1	,'2020-10-07')
	,(376	,22	,2	,1	,'2020-10-07')
	,(479	,18	,2	,1	,'2020-10-09')
	,(372	,16	,3	,1	,'2020-10-11')
	,(118	,17	,1	,1	,'2020-10-12')
	,(598	,14	,3	,1	,'2020-10-15')
	,(151	,11	,3	,1	,'2020-10-18')
	,(250	,22	,2	,1	,'2020-10-22')
	,(507	,1	,2	,1	,'2020-10-26')
	,(65		,2	,1	,1	,'2020-10-28')	  
	,(266	,8	,1	,1	,'2020-10-28')
	,(324	,11	,1	,1	,'2020-10-29')
	,(546	,4	,2	,1	,'2020-10-29')
	,(97		,13	,3	,1	,'2020-11-06')	  
	,(39		,7	,1	,1	,'2020-11-10')	  
	,(549	,11	,2	,1	,'2020-11-12')
	,(75		,2	,3	,1	,'2020-11-13')	  
	,(342	,14	,1	,1	,'2020-11-14')
	,(16		,23	,3	,1	,'2020-11-17')	  
	,(290	,10	,2	,1	,'2020-11-17')
	,(554	,9	,3	,1	,'2020-11-18')
	,(6		,12	,2	,1	,'2020-11-20')	  
	,(5		,11	,1	,1	,'2020-11-23')	  
	,(477	,16	,1	,1	,'2020-11-26')
	,(596	,13	,2	,1	,'2020-11-27')
	,(327	,22	,3	,1	,'2020-12-08')
	,(410	,11	,1	,1	,'2020-12-09')
	,(411	,21	,2	,1	,'2020-12-10')
	,(384	,20	,1	,1	,'2020-12-12')
	,(190	,22	,1	,1	,'2020-12-13')
	,(523	,4	,3	,1	,'2020-12-14')
	,(264	,6	,3	,1	,'2020-12-16')
	,(3		,23	,3	,1	,'2020-12-17')	  
	,(268	,7	,2	,1	,'2020-12-17')
	,(363	,23	,2	,1	,'2020-12-17')
	,(289	,2	,2	,1	,'2020-12-25')
	,(201	,16	,1	,1	,'2020-12-26')
	,(358	,15	,2	,1	,'2020-12-30')
	,(129	,9	,1	,1	,'2020-12-31')
	,(373	,7	,2	,1	,'2021-01-01')
	,(340	,8	,3	,1	,'2021-01-05')
	,(532	,12	,2	,1	,'2021-01-05')
	,(604	,4	,3	,1	,'2021-01-05')
	,(578	,1	,1	,1	,'2021-01-08')
	,(146	,4	,1	,1	,'2021-01-10')
	,(66		,15	,1	,1	,'2021-01-11')	  
	,(471	,6	,3	,1	,'2021-01-13')
	,(503	,21	,1	,1	,'2021-01-13')
	,(123	,14	,2	,1	,'2021-01-15')
	,(186	,4	,1	,1	,'2021-01-18')
	,(586	,22	,3	,1	,'2021-01-18')
	,(297	,22	,3	,1	,'2021-01-21')
	,(236	,15	,3	,1	,'2021-01-22')
	,(183	,13	,3	,1	,'2021-01-23')
	,(347	,17	,3	,1	,'2021-01-23')
	,(476	,16	,2	,1	,'2021-01-23')
	,(159	,11	,1	,1	,'2021-01-27')
	,(382	,5	,3	,1	,'2021-02-02')
	,(361	,21	,1	,1	,'2021-02-05')
	,(28		,19	,1	,1	,'2021-02-09')	  
	,(505	,3	,2	,1	,'2021-02-10')
	,(92		,22	,2	,1	,'2021-02-11')	  
	,(437	,5	,2	,1	,'2021-02-15')
	,(616	,23	,2	,1	,'2021-02-15')
	,(348	,23	,2	,1	,'2021-02-17')
	,(438	,9	,1	,1	,'2021-02-19')
	,(43		,13	,2	,1	,'2021-02-21')	  
	,(212	,18	,3	,1	,'2021-02-26')
	,(325	,11	,3	,1	,'2021-02-28')
	,(469	,4	,3	,1	,'2021-03-01')
	,(538	,2	,2	,1	,'2021-03-01')
	,(94		,6	,2	,1	,'2021-03-03')	  
	,(407	,13	,2	,1	,'2021-03-07')
	,(261	,1	,3	,1	,'2021-03-11')
	,(396	,20	,1	,1	,'2021-03-11')
	,(541	,15	,1	,1	,'2021-03-30')
	,(494	,10	,3	,1	,'2021-03-31')
	,(524	,2	,3	,1	,'2021-03-31')
	,(143	,1	,3	,1	,'2021-04-03')
	,(50		,12	,1	,1	,'2021-04-04')	  
	,(460	,21	,3	,1	,'2021-04-04')
	,(184	,12	,1	,1	,'2021-04-05')
	,(258	,1	,3	,1	,'2021-04-07')
	,(456	,7	,3	,1	,'2021-04-10')
	,(130	,13	,1	,1	,'2021-04-18')
	,(542	,4	,1	,1	,'2021-04-19')
	,(379	,16	,3	,1	,'2021-04-21')
	,(428	,6	,1	,1	,'2021-04-21')
	,(390	,16	,1	,1	,'2021-04-22')
	,(405	,23	,3	,1	,'2021-04-22')
	,(314	,4	,3	,1	,'2021-04-26')
	,(63		,20	,1	,1	,'2021-04-27')	  
	,(591	,4	,3	,1	,'2021-04-28')
	,(124	,8	,2	,1	,'2021-04-29')
	,(188	,15	,3	,1	,'2021-04-29')
	,(508	,5	,2	,1	,'2021-05-01')
	,(35		,16	,2	,1	,'2021-05-06')	  
	,(42		,9	,1	,1	,'2021-05-06')	  
	,(530	,22	,1	,1	,'2021-05-06')
	,(362	,5	,2	,1	,'2021-05-07')
	,(386	,7	,3	,1	,'2021-05-10')
	,(603	,17	,3	,1	,'2021-05-11')
	,(590	,18	,1	,1	,'2021-05-12')
	,(452	,3	,3	,1	,'2021-05-13')
	,(139	,9	,3	,1	,'2021-05-19')
	,(364	,10	,1	,1	,'2021-05-20')
	,(353	,11	,1	,1	,'2021-05-21')
	,(496	,15	,3	,1	,'2021-05-24')
	,(237	,23	,3	,1	,'2021-05-25')
	,(539	,14	,3	,1	,'2021-05-25')
	,(293	,10	,1	,1	,'2021-05-26')
	,(389	,12	,2	,1	,'2021-05-27')
	,(615	,1	,2	,1	,'2021-05-28')
	,(553	,19	,3	,1	,'2021-05-30')
	,(409	,1	,1	,1	,'2021-06-01')
	,(506	,21	,3	,1	,'2021-06-01')
	,(83		,14	,2	,1	,'2021-06-02')	  
	,(1		,12	,1	,1	,'2021-06-03')	  
	,(335	,21	,2	,1	,'2021-06-03')
	,(528	,16	,2	,1	,'2021-06-03')
	,(311	,7	,3	,1	,'2021-06-12')
	,(475	,13	,3	,1	,'2021-06-14')
	,(536	,16	,1	,1	,'2021-06-14')
	,(213	,8	,1	,1	,'2021-06-15')
	,(126	,19	,1	,1	,'2021-06-16')
	,(121	,10	,1	,1	,'2021-06-17')
	,(404	,9	,1	,1	,'2021-06-17')
	,(499	,23	,2	,1	,'2021-06-21')
	,(585	,18	,3	,1	,'2021-06-24')
	,(525	,1	,1	,1	,'2021-06-26')
	,(246	,16	,1	,1	,'2021-06-27')
	,(350	,4	,1	,1	,'2021-06-30')
	,(2		,15	,3	,1	,'2021-07-04')	  
	,(239	,16	,2	,1	,'2021-07-04')
	,(282	,6	,2	,1	,'2021-07-06')
	,(454	,5	,3	,1	,'2021-07-06')
	,(618	,2	,1	,1	,'2021-07-07')
	,(134	,23	,1	,1	,'2021-07-08')
	,(408	,21	,1	,1	,'2021-07-10')
	,(60		,15	,1	,1	,'2021-07-21')	  
	,(17		,5	,1	,1	,'2021-07-22')	  
	,(40		,9	,1	,1	,'2021-07-26')	  
	,(56		,2	,2	,1	,'2021-07-27')	  
	,(300	,21	,3	,1	,'2021-07-31')
	,(9		,14	,1	,1	,'2021-08-03')	  
	,(394	,14	,1	,1	,'2021-08-04')
	,(344	,17	,2	,1	,'2021-08-06')
	,(150	,22	,1	,1	,'2021-08-07')
	,(301	,5	,3	,1	,'2021-08-07')
	,(36		,3	,2	,1	,'2021-08-19')	  
	,(594	,16	,2	,1	,'2021-08-22')
	,(19		,11	,2	,1	,'2021-08-23')	  
	,(391	,10	,3	,1	,'2021-08-25')
	,(80		,19	,2	,1	,'2021-08-27')	  
	,(279	,7	,1	,1	,'2021-08-28')
	,(426	,20	,1	,1	,'2021-08-28')
	,(252	,5	,2	,1	,'2021-08-30')
	,(473	,6	,2	,1	,'2021-09-01')
	,(208	,5	,3	,1	,'2021-09-03')
	,(265	,1	,2	,1	,'2021-09-07')
	,(352	,14	,3	,1	,'2021-09-12')
	,(165	,17	,2	,1	,'2021-09-13')
	,(365	,15	,3	,1	,'2021-09-13')
	,(354	,22	,1	,1	,'2021-09-16')
	,(132	,1	,1	,1	,'2021-09-18')
	,(620	,10	,1	,1	,'2021-09-19')
	,(332	,23	,1	,1	,'2021-09-20')
	,(257	,4	,1	,1	,'2021-09-22')
	,(33		,4	,3	,1	,'2021-09-23')	  
	,(59		,20	,1	,1	,'2021-09-24')	  
	,(81		,20	,2	,1	,'2021-09-27')	  
	,(24		,10	,2	,1	,'2021-10-03')	  
	,(196	,14	,1	,1	,'2021-10-06')
	,(269	,22	,2	,1	,'2021-10-06')
	,(160	,18	,1	,1	,'2021-10-08')
	,(71		,23	,2	,1	,'2021-10-09')	  
	,(217	,1	,1	,1	,'2021-10-09')
	,(424	,6	,3	,1	,'2021-10-13')
	,(117	,7	,1	,1	,'2021-10-14')
	,(49		,2	,3	,1	,'2021-10-16')	  
	,(167	,13	,3	,1	,'2021-10-22')
	,(26		,1	,3	,1	,'2021-10-23')	  
	,(274	,15	,2	,1	,'2021-10-27')
	,(582	,12	,3	,1	,'2021-10-27')
	,(292	,4	,2	,1	,'2021-10-28')
	,(10		,19	,2	,1	,'2021-10-31')	  
	,(241	,14	,3	,1	,'2021-11-05')
	,(333	,5	,2	,1	,'2021-11-06')
	,(357	,7	,1	,1	,'2021-11-07')
	,(61		,14	,2	,1	,'2021-11-10')	  
	,(77		,3	,2	,1	,'2021-11-11')	  
	,(207	,17	,1	,1	,'2021-11-11')
	,(617	,3	,2	,1	,'2021-11-12')
	,(214	,20	,3	,1	,'2021-11-13')
	,(235	,23	,3	,1	,'2021-11-14')
	,(277	,4	,1	,1	,'2021-11-18')
	,(331	,22	,3	,1	,'2021-11-20')
	,(577	,12	,3	,1	,'2021-11-20')
	,(227	,5	,2	,1	,'2021-11-22')
	,(116	,4	,3	,1	,'2021-11-24')
	,(543	,3	,2	,1	,'2021-11-26')
	,(115	,4	,1	,1	,'2021-11-29')
	,(321	,17	,1	,1	,'2021-12-03')
	,(30		,2	,2	,1	,'2021-12-07')	  
	,(46		,21	,3	,1	,'2021-12-08')	  
	,(249	,14	,1	,1	,'2021-12-09')
	,(137	,5	,3	,1	,'2021-12-16')
	,(310	,7	,1	,1	,'2021-12-16')
	,(501	,8	,2	,1	,'2021-12-16')
	,(112	,3	,3	,1	,'2021-12-18')
	,(323	,3	,3	,1	,'2021-12-21')
	,(345	,6	,3	,1	,'2021-12-23')
	,(125	,18	,2	,1	,'2021-12-24')
	,(51		,8	,1	,1	,'2021-12-25')	  
	,(185	,4	,3	,1	,'2021-12-25')
	,(242	,19	,3	,1	,'2021-12-25')
	,(544	,23	,2	,1	,'2021-12-25')
	,(451	,11	,1	,1	,'2021-12-27')
	,(100	,23	,3	,1	,'2021-12-28')
	,(148	,4	,2	,1	,'2021-12-28')
	,(27		,2	,2	,1	,'2022-01-02')	  
	,(13		,8	,3	,1	,'2022-01-03')	  
	,(556	,3	,1	,1	,'2022-01-03')
	,(302	,23	,1	,1	,'2022-01-04')
	,(614	,6	,2	,1	,'2022-01-11')
	,(375	,4	,1	,1	,'2022-01-16')
	,(14		,20	,1	,1	,'2022-01-17')	  
	,(463	,11	,1	,1	,'2022-01-17')
	,(328	,18	,2	,1	,'2022-01-21')
	,(457	,19	,3	,1	,'2022-01-22')
	,(580	,6	,3	,1	,'2022-01-22')
	,(529	,15	,2	,1	,'2022-01-24')
	,(552	,20	,3	,1	,'2022-01-25')
	,(64		,4	,1	,1	,'2022-01-27')	  
	,(131	,16	,3	,1	,'2022-01-28')
	,(607	,23	,1	,1	,'2022-02-01')
	,(495	,3	,2	,1	,'2022-02-07')
	,(149	,6	,3	,1	,'2022-02-11')
	,(58		,18	,1	,1	,'2022-02-16')	  
	,(101	,4	,2	,1	,'2022-02-17')
	,(559	,7	,3	,1	,'2022-02-18')
	,(18		,15	,1	,1	,'2022-02-19')	  
	,(176	,19	,3	,1	,'2022-02-19')
	,(427	,5	,1	,1	,'2022-02-19')
	,(318	,10	,2	,1	,'2022-02-20')
	,(449	,3	,2	,1	,'2022-02-20')
	,(597	,13	,3	,1	,'2022-02-20')
	,(468	,7	,1	,1	,'2022-02-21')
	,(587	,19	,1	,1	,'2022-02-22')
	,(551	,3	,3	,1	,'2022-03-05')
	,(557	,3	,3	,1	,'2022-03-05')
	,(298	,19	,1	,1	,'2022-03-07')
	,(467	,8	,3	,1	,'2022-03-07')
	,(374	,15	,2	,1	,'2022-03-08')
	,(497	,21	,1	,1	,'2022-03-11')
	,(606	,21	,1	,1	,'2022-03-13')
	,(304	,12	,1	,1	,'2022-03-15')
	,(548	,12	,1	,1	,'2022-03-15')
	,(163	,17	,3	,1	,'2022-03-26')
	,(175	,13	,3	,1	,'2022-03-26')
	,(291	,8	,1	,1	,'2022-03-26')
	,(545	,22	,2	,1	,'2022-03-30')
	,(62		,21	,3	,1	,'2022-04-01')	  
	,(231	,5	,3	,1	,'2022-04-02')
	,(435	,11	,1	,1	,'2022-04-07')
	,(243	,7	,1	,1	,'2022-04-08')
	,(109	,7	,2	,1	,'2022-04-11')
	,(307	,9	,1	,1	,'2022-04-11')
	,(308	,6	,1	,1	,'2022-04-11')
	,(360	,23	,3	,1	,'2022-04-17')
	,(113	,4	,3	,1	,'2022-04-21')
	,(198	,12	,1	,1	,'2022-04-21')
	,(67		,12	,1	,1	,'2022-04-23')	  
	,(319	,4	,1	,1	,'2022-04-27')
	,(406	,21	,2	,1	,'2022-04-27')
	,(621	,18	,1	,1	,'2022-04-29')
	,(218	,18	,3	,1	,'2022-05-03')
	,(312	,3	,2	,1	,'2022-05-03')
	,(85		,1	,2	,1	,'2022-05-04')
	,(263	,8	,2	,1	,'2022-05-04')
	,(401	,2	,2	,1	,'2022-05-06')
	,(86		,15	,1	,1	,'2022-05-12')
	,(531	,13	,2	,1	,'2022-05-12')
	,(95		,2	,2	,1	,'2022-05-15')
	,(20		,4	,2	,1	,'2022-05-16')
	,(145	,9	,3	,1	,'2022-05-17')
	,(593	,10	,1	,1	,'2022-05-19')
	,(72		,12	,3	,1	,'2022-05-24')
	,(226	,7	,1	,1	,'2022-05-24')
	,(500	,7	,1	,1	,'2022-05-24')
	,(550	,16	,2	,1	,'2022-05-27')
	,(560	,6	,1	,1	,'2022-05-28')
	,(498	,2	,2	,1	,'2022-05-29')
	,(601	,19	,2	,1	,'2022-05-29')
	,(432	,21	,2	,1	,'2022-05-31')
	,(280	,11	,3	,1	,'2022-06-04')
	,(431	,9	,2	,1	,'2022-06-05')
	,(102	,3	,3	,1	,'2022-06-06')
	,(187	,10	,2	,1	,'2022-06-06')
	,(555	,1	,2	,1	,'2022-06-07')
	,(4		,9	,3	,1	,'2022-06-08')
	,(164	,16	,3	,1	,'2022-06-13')
	,(197	,19	,2	,1	,'2022-06-13')
	,(96		,3	,3	,1	,'2022-06-15')
	,(313	,14	,3	,1	,'2022-06-15')
	,(329	,9	,3	,1	,'2022-06-18')
	,(537	,8	,1	,1	,'2022-06-19')
	,(299	,5	,1	,1	,'2022-06-21')
	,(68		,14	,2	,1	,'2022-06-24')
	,(283	,17	,1	,1	,'2022-06-24')
	,(234	,8	,1	,1	,'2022-06-28')
	,(25		,13	,2	,1	,'2022-06-30')
	,(203	,3	,1	,1	,'2022-06-30')
	,(399	,16	,1	,1	,'2022-06-30')
	,(397	,13	,2	,1	,'2022-07-06')
	,(232	,7	,3	,1	,'2022-07-07')
	,(464	,14	,1	,1	,'2022-07-11')
	,(608	,8	,2	,1	,'2022-07-11')
	,(191	,8	,2	,1	,'2022-07-13')
	,(240	,18	,1	,1	,'2022-07-13')
	,(93		,20	,2	,1	,'2022-07-14')
	,(7		,4	,2	,1	,'2022-07-16')
	,(330	,10	,3	,1	,'2022-07-17')
	,(12		,13	,1	,1	,'2022-07-24')
	,(346	,12	,2	,1	,'2022-07-24')
	,(613	,16	,2	,1	,'2022-07-29')
	,(455	,10	,2	,1	,'2022-07-31')
	,(611	,12	,2	,1	,'2022-07-31')
	,(296	,1	,3	,1	,'2022-08-01')
	,(210	,22	,2	,1	,'2022-08-05')
	,(216	,19	,2	,1	,'2022-08-05')
	,(22		,10	,2	,1	,'2022-08-09')
	,(147	,3	,2	,1	,'2022-08-09')
	,(366	,21	,1	,1	,'2022-08-10')
	,(206	,3	,1	,1	,'2022-08-15')
	,(114	,22	,2	,1	,'2022-08-19')
	,(334	,1	,3	,1	,'2022-08-19')
	,(400	,17	,2	,1	,'2022-08-23')
	,(533	,17	,2	,1	,'2022-08-23')
	,(429	,19	,3	,1	,'2022-08-24')
	,(260	,1	,3	,1	,'2022-08-25')
	,(381	,9	,2	,1	,'2022-08-25')
	,(436	,10	,1	,1	,'2022-08-26')
	,(70	,15	,2	,1	,'2022-08-28')
	,(540	,23	,1	,1	,'2022-09-01')
	,(581	,5	,2	,1	,'2022-09-01')
	,(200	,22	,2	,1	,'2022-09-04')
	,(478	,4	,2	,1	,'2022-09-04')
	,(295	,1	,1	,1	,'2022-09-05')
	,(271	,21	,3	,1	,'2022-09-09')
	,(8		,15	,1	,1	,'2022-09-12')
	,(610	,16	,3	,1	,'2022-09-14')
	,(341	,1	,2	,1	,'2022-09-19')
	,(398	,2	,3	,1	,'2022-09-20')
	,(87	,20	,2	,1	,'2022-09-28')
	,(205	,19	,2	,1	,'2022-09-28')
	,(595	,14	,1	,1	,'2022-09-28')
	,(251	,20	,3	,1	,'2022-09-30')
	,(152	,7	,1	,1	,'2022-10-02')
	,(385	,11	,1	,1	,'2022-10-03')
	,(161	,7	,3	,1	,'2022-10-07')
	,(602	,21	,1	,1	,'2022-10-11')
	,(119	,11	,1	,1	,'2022-10-14')
	,(229	,17	,1	,1	,'2022-10-14')
	,(383	,8	,1	,1	,'2022-10-14')
	,(403	,18	,3	,1	,'2022-10-14')
	,(136	,14	,1	,1	,'2022-10-19')
	,(579	,22	,3	,1	,'2022-10-21')
	,(128	,17	,3	,1	,'2022-10-25')
	,(504	,15	,3	,1	,'2022-10-28')
	,(195	,16	,3	,1	,'2022-10-29')
	,(111	,15	,3	,1	,'2022-11-03')
	,(273	,2	,1	,1	,'2022-11-03')
	,(45	,20	,1	,1	,'2022-11-05')
	,(182	,17	,2	,1	,'2022-11-05')
	,(276	,7	,3	,1	,'2022-11-05')
	,(599	,5	,1	,1	,'2022-11-06')
	,(262	,15	,1	,1	,'2022-11-07')
	,(315	,12	,2	,1	,'2022-11-08')
	,(21	,22	,2	,1	,'2022-11-11')
	,(108	,11	,1	,1	,'2022-11-11')
	,(339	,19	,1	,1	,'2022-11-12')
	,(388	,16	,2	,1	,'2022-11-13')
	,(402	,13	,2	,1	,'2022-11-13')
	,(378	,14	,3	,1	,'2022-11-14')
	,(178	,23	,3	,1	,'2022-11-16')
	,(309	,4	,1	,1	,'2022-11-16')
	,(31	,5	,2	,1	,'2022-11-18')

INSERT INTO payments
	VALUES
	(1		,'4880198552044300'	,'2022-11-01'	,'571'	,'visa'		 )
	,(2		,'4741219046651610'	,'2022-11-01'	,'379'	,'visa'		 )
	,(3		,'4005868638169720'	,'2023-02-01'	,'764'	,'mastercard')
	,(4		,'4731933933054040'	,'2023-03-01'	,'867'	,'mastercard')
	,(5		,'4704132702366490'	,'2023-03-01'	,'307'	,'visa'		 )
	,(6		,'4362201477405170'	,'2023-03-01'	,'605'	,'mastercard')
	,(7		,'4977523909032440'	,'2023-04-01'	,'675'	,'visa'		 )
	,(8		,'4473394500631820'	,'2023-09-01'	,'197'	,'mastercard')
	,(9		,'4670781862634590'	,'2023-12-01'	,'278'	,'visa'		 )
	,(10	,'4737012348603650'	,'2023-12-01'	,'516'	,'visa'		 )
	,(11	,'4972733435946230'	,'2024-10-01'	,'829'	,'visa'		 )
	,(12	,'4937640485203880'	,'2024-12-01'	,'676'	,'mastercard')
	,(13	,'4442963487515400'	,'2024-12-01'	,'387'	,'mastercard')
	,(14	,'4473173801434180'	,'2025-01-01'	,'090'	,'mastercard')
	,(15	,'4244017271929730'	,'2025-01-01'	,'048'	,'mastercard')
	,(16	,'4335562857243950'	,'2025-10-01'	,'014'	,'mastercard')
	,(17	,'4141270365419340'	,'2026-02-01'	,'402'	,'visa'		 )
	,(18	,'4474515854877150'	,'2026-02-01'	,'392'	,'visa'		 )
	,(19	,'4611594145437300'	,'2026-05-01'	,'192'	,'mastercard')
	,(20	,'4284450340129820'	,'2028-08-01'	,'904'	,'visa'		 )
	,(21	,'4754605042419380'	,'2028-09-01'	,'061'	,'visa'		 )
	,(22	,'4957031685845820'	,'2028-10-01'	,'779'	,'mastercard')
	,(23	,'4276162657410980'	,'2028-11-01'	,'175'	,'mastercard')


--------------------------------
-- Create Views for Reporting --
--------------------------------

-- Shows all employee data in one table
DROP VIEW IF EXISTS v_employees_departments_positions
IF NOT EXISTS (SELECT 1 FROM SYS.VIEWS WHERE NAME = 'v_employees_departments_positions')
BEGIN
	EXEC(
		 '
		 CREATE VIEW v_employees_departments_positions AS 
			SELECT 
				e.*
				, p.position_name
				, p.position_pay_rate
				, d.department_name
			FROM employees e 
				JOIN positions p 
				ON e.employee_position = p.position_code
				JOIN departments d
				ON e.employee_department = d.department_code
		'
		)
END
GO

-- Shows gross revenue grouped by product
DROP VIEW IF EXISTS v_gross_revenue_by_product_to_date  -- were we calculate the 'order_totals' for the dashboard
IF NOT EXISTS (SELECT 1 FROM SYS.VIEWS WHERE NAME = 'v_gross_revenue_by_product_to_date')
BEGIN
	EXEC('
		CREATE VIEW v_gross_revenue_by_product_to_date AS
		SELECT
			p.product_name
			,AVG(p.product_price) AS product_unit_price
			,SUM(o.order_quantity) AS total_quantity_sold
			,SUM(CAST(o.order_quantity * p.product_price AS MONEY)) AS order_total
		FROM orders o
		JOIN bottles b
			ON o.order_bottle_id = b.bottle_id
		JOIN products p
			ON b.bottle_product_id = p.product_id
		GROUP BY 
			p.product_name
		 '
		)
END
GO

-- Shows total revenue grouped by individual customer and the products they bought
DROP VIEW IF EXISTS v_revenue_per_customer_to_date_by_product  -- were we calculate the 'order_totals' for the dashboard
IF NOT EXISTS (SELECT 1 FROM SYS.VIEWS WHERE NAME = 'v_revenue_per_customer_to_date_by_product')
BEGIN
	EXEC('
		CREATE VIEW v_revenue_per_customer_to_date_by_product AS
		SELECT
			c.customer_lastname
			,c.customer_firstname
			,p.product_name
			,COUNT(c.customer_id) AS total_orders
			,SUM(o.order_quantity * p.product_price) AS total
			,c.customer_email
		FROM customers c
		JOIN orders o ON o.order_customer_id = c.customer_id
		JOIN bottles b ON b.bottle_id = o.order_bottle_id
		JOIN products p ON  p.product_id = b.bottle_product_id
		GROUP BY
			c.customer_lastname
			,c.customer_firstname
			,p.product_name
			,CONCAT(c.customer_lastname, c.customer_firstname)
			,c.customer_email
		'
		)
END
GO



---------------------------------
-- Views (External Data Model) --
---------------------------------

-------------------------------
--Sintia (Sales) User Stories--
-------------------------------

--As a user, I would like to see how many chardonnay bottles we have in stock
-----------------------------------------------------------------------------
DECLARE @total_bottles AS INT
SET @total_bottles = (
	SELECT
		COUNT(b.bottle_id)
	FROM bottles b 
	INNER JOIN products p ON b.bottle_product_id = p.product_id
)

SELECT @total_bottles
GO


DECLARE @num_bottles_sold AS INT
SET @num_bottles_sold = (
	SELECT
		COUNT(b.bottle_id)
	FROM bottles b 
	INNER JOIN products p ON b.bottle_product_id = p.product_id
	WHERE b.bottle_sold = 1
)

SELECT @num_bottles_sold
GO

CREATE VIEW v_bottles_in_stock_by_product AS (
	SELECT 
		p.product_name
		, COUNT(b.bottle_id) as bottles_in_stock
	FROM products p 
	INNER JOIN bottles b ON b.bottle_product_id = p.product_id
	WHERE b.bottle_sold = 0
	GROUP BY p.product_name
)
GO

-- view to see all bottles, sold or not

CREATE VIEW v_bottles_sold_by_product AS (
	SELECT 
		p.product_name
		, COUNT(b.bottle_id) as bottles_sold
	FROM products p 
	INNER JOIN bottles b ON b.bottle_product_id = p.product_id
	WHERE b.bottle_sold = 1
	GROUP BY p.product_name
)
GO



CREATE VIEW v_bottles_sold_and_in_stock AS (
	SELECT 
		a.product_name
		, a.bottles_in_stock
		, b.bottles_sold
	FROM v_bottles_in_stock_by_product a 
	JOIN v_bottles_sold_by_product b ON a.product_name = b.product_name
)
GO



--As a user, I would like to see how many orders we have had in 2022 and for what products
------------------------------------------------------------------------------------------
CREATE VIEW v_product_order_count_by_year AS (
	SELECT
		YEAR(o.order_date) AS year
		, COUNT(o.order_id) AS order_count
		, p.product_name
	FROM products p
	JOIN bottles b ON b.bottle_product_id = p.product_id
	JOIN orders o ON o.order_bottle_id = b.bottle_id
	GROUP BY
		YEAR(o.order_date)
		, p.product_name
)
GO



--As a user, I want to see our top seller products
--------------------------------------------------

CREATE VIEW v_top_selling_products AS
	SELECT
		COUNT(b.bottle_id) AS bottles_sold_to_date
		, p.product_name
	FROM bottles b
	JOIN products p ON p.product_id = b.bottle_product_id
	GROUP BY p.product_name
GO


-----------------------------------
--Bryan (Operations) User Stories--
-----------------------------------


-- As a business analyst, I can track a grape from the vineyard to the bottle, for quality assurance purposes. 
---------------------------------------------------------------------------------------------------------------
-- tracking would happen like: 'bottle' -> 'barrel' -> 'fermenter' -> 'vineyard' (harvest date + etc...)

CREATE VIEW v_vine_to_bottle AS
	SELECT 
		v.vineyard_id
		, v.vineyard_employee_id
		, g.grape_name
		, fv.fermenter_id
		, f.fermenter_employee_id
		, fv.fermenter_start_date
		, fv.fermenter_end_date
		, b1.barrel_number
		, b1.barrel_cellar_id
		, b1.barrel_start_date
		, b1.barrel_end_date
		, b2.bottle_label_number
		, b2.bottle_fill_date
		, o.order_id
		, o.order_shipper_id
		, c.customer_id
		, c.customer_email
	FROM vineyards v
	JOIN grapes g ON g.grape_id = v.vineyard_grape_id
	JOIN fermenters_vineyards fv ON fv.fermenter_vineyard_id = v.vineyard_id
	JOIN fermenters f ON fv.fermenter_id = f.fermenter_id
	JOIN barrels b1 ON b1.barrel_fermenter_id = fv.fermenter_id
	JOIN bottles b2 ON b2.bottle_barrel_id = b1.barrel_id
	JOIN orders o ON o.order_bottle_id = b2.bottle_id
	JOIN customers c ON c.customer_id = o.order_customer_id
	WHERE b2.bottle_sold = 1
GO

 

-- As a business analyst, I can see how long a product was aged for, to ensure that the same product is being aged consistently. 
---------------------------------------------------------------------------------------------------------------------------------

CREATE VIEW v_product_aging AS
	SELECT 
		p.product_name
		, b2.barrel_number
		, b2.barrel_start_date
		, b2.barrel_end_date
	FROM products p
	JOIN bottles b1 ON b1.bottle_product_id = p.product_id
	JOIN barrels b2 ON b2.barrel_id = b1.bottle_barrel_id
GO


-- As a business analyst, I can see when it a product was bottled, for quality assurance/recall purposes in case there was an issue on the bottling line.
-----------------------------------------------------------------------------------------------------------------------------------------------------------
-- USE v_vine_to_bottle for this (can get the same thing)
 

-- As a business analyst, I can see how long each product underwent the fermentation process and which fermenter was used, for quality assurance purposes.
-----------------------------------------------------------------------------------------------------------------------------------------------------------
-- USE v_vine_to_bottle for this (can get the same thing)



-- Total sales by product




CREATE VIEW v_sales_revenue AS (
	SELECT
		p.product_name
		, p.product_price
		, v1.bottles_sold
	FROM products p
	JOIN v_bottles_sold_and_in_stock v1 ON p.product_name = v1.product_name
)
GO



