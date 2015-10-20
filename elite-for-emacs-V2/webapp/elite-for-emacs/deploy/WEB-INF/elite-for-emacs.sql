--Elite for EMACS database for MySQL

drop database if exists eliteforemacs;

create database if not exists eliteforemacs;

use eliteforemacs;

--commander table
create table if not exists commanders (name varchar(255),data mediumtext,created timestamp );

--market table
create table if not exists markets (galaxy integer,planet integer, data varchar(255) );

--fluct
create table if not exists fluct (fluct integer,lastrand integer );
--initial fluct
insert into fluct values (0,0);

--messages
create table if not exists messages (galaxy integer,planet integer, commander varchar(255), message varchar(160),sent timestamp );

--messages for galaxy, users can not send, todo:only via elite federation
create table if not exists galaxymessages (galaxy integer, message varchar(160),sent timestamp );

--messages for universum, users can not send, todo:only via elite federation
create table if not exists universummessages (message varchar(160),sent timestamp );

