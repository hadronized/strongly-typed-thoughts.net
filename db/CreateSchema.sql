-- Create the database and its tables

-- Credentials
create table Credentials (
    credID   Integer primary key
  , credName VarChar(64) not null unique
  , credPwd  Bit(512) not null
);
