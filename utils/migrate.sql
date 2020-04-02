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

