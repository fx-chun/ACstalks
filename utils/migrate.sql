-- alpha

CREATE TABLE IF NOT EXISTS HotItems_Schema1 (
    HotItemID INTEGER PRIMARY KEY,
    HotItem varchar(255) NOT NULL,
    HotItemTime datetime NOT NULL,
    HotItemTimezone varchar(5) NOT NULL,
    UserID int NOT NULL
);

CREATE TABLE IF NOT EXISTS Prices_Schema1 (
    PriceID INTEGER PRIMARY KEY,
    Price int NOT NULL,
    PriceTime datetime NOT NULL,
    PriceTimezone varchar(5) NOT NULL,
    UserID int NOT NULL
);

CREATE TABLE IF NOT EXISTS Tokens_Schema1 (
    Token varchar(255) NOT NULL,
    UserID int NOT NULL
);

CREATE TABLE IF NOT EXISTS Users_Schema1 (
    UserID INTEGER PRIMARY KEY, 
    Username varchar(255) NOT NULL UNIQUE,
    Nickname varchar(255) NOT NULL,
    PassHash varchar(255) NOT NULL,
    SecurityAnswer varchar(255) NOT NULL,
    SwitchFC varchar(17) NOT NULL,
    DodoCode varchar(5) NOT NULL,
    IslandOpen varchar(32) NOT NULL,
    IslandOpenTime datetime NOT NULL,
    Bio varchar(512) NOT NULL,
    FavVillager varchar(255) NOT NULL,
    FavThing varchar(255) NOT NULL
);

-- v1.0.0

CREATE TABLE IF NOT EXISTS HotItems_Schema2 (
    HotItemID INTEGER PRIMARY KEY,
    HotItem varchar(255) NOT NULL,
    HotItemTime datetime NOT NULL,
    HotItemTimezone varchar(5) NOT NULL,
    UserID int NOT NULL

    -- new
);

INSERT INTO HotItems_Schema2 SELECT * FROM HotItems_Schema1;
DROP TABLE HotItems_Schema1; 

CREATE TABLE IF NOT EXISTS Prices_Schema2 (
    PriceID INTEGER PRIMARY KEY,
    Price int NOT NULL,
    PriceTime datetime NOT NULL,
    PriceTimezone varchar(5) NOT NULL,
    UserID int NOT NULL

    -- new
);

INSERT INTO Prices_Schema2 SELECT * FROM Prices_Schema1;
DROP TABLE Prices_Schema1;

CREATE TABLE IF NOT EXISTS Tokens_Schema2 (
    Token varchar(255) NOT NULL,
    UserID int NOT NULL

    -- new
);

INSERT INTO Tokens_Schema2 SELECT * FROM Tokens_Schema1;
DROP TABLE Tokens_Schema1;

CREATE TABLE IF NOT EXISTS Users_Schema2 (
    UserID INTEGER PRIMARY KEY,
    Username varchar(255) NOT NULL UNIQUE,
    Nickname varchar(255) NOT NULL,
    PassHash varchar(255) NOT NULL,
    SecurityAnswer varchar(255) NOT NULL,
    SwitchFC varchar(17) NOT NULL,
    DodoCode varchar(5) NOT NULL,
    IslandOpen varchar(32) NOT NULL,
    IslandOpenTime datetime NOT NULL,
    Bio varchar(512) NOT NULL,
    FavVillager varchar(255) NOT NULL,
    FavThing varchar(255) NOT NULL,
    
    -- new
    NativeFruit varchar(255) NOT NULL 
);

INSERT INTO Users_Schema2 SELECT *, "n/a" FROM Users_Schema1;
DROP TABLE Users_Schema1;


