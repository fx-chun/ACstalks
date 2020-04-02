#!/bin/sh

sqlite3 database.db ".read utils/migrate.sql"
