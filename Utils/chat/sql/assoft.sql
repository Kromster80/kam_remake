-- phpMyAdmin SQL Dump
-- version 3.3.7
-- http://www.phpmyadmin.net
--
-- Host: localhost
-- Generation Time: Oct 20, 2010 at 10:00 PM
-- Server version: 5.0.41
-- PHP Version: 5.2.5

SET SQL_MODE="NO_AUTO_VALUE_ON_ZERO";


/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8 */;

--
-- Database: `assoft`
--

-- --------------------------------------------------------

--
-- Table structure for table `as_chat_room`
--

DROP TABLE IF EXISTS `as_chat_room`;
CREATE TABLE IF NOT EXISTS `as_chat_room` (
  `room_id` int(63) NOT NULL auto_increment,
  `host_id` int(63) NOT NULL,
  `host_name` varchar(255) NOT NULL,
  `room_name` varchar(255) NOT NULL,
  `tag` int(16) NOT NULL,
  PRIMARY KEY  (`room_id`),
  UNIQUE KEY `room_name` (`room_name`)
) ENGINE=InnoDB DEFAULT CHARSET=cp1251 AUTO_INCREMENT=4 ;

--
-- Dumping data for table `as_chat_room`
--

INSERT INTO `as_chat_room` VALUES(1, 24, '222', 'qqq', 0);
INSERT INTO `as_chat_room` VALUES(2, 24, '222', 'qqq1', 0);
INSERT INTO `as_chat_room` VALUES(3, 25, '2224', 'qqq12', 0);

-- --------------------------------------------------------

--
-- Table structure for table `as_users`
--

DROP TABLE IF EXISTS `as_users`;
CREATE TABLE IF NOT EXISTS `as_users` (
  `user_id` bigint(20) NOT NULL auto_increment,
  `user_name` varchar(255) NOT NULL,
  `password` varchar(255) NOT NULL,
  `ip` varchar(16) NOT NULL,
  `last_visit` timestamp NOT NULL default CURRENT_TIMESTAMP,
  `tag` int(11) NOT NULL default '0',
  PRIMARY KEY  (`user_id`),
  UNIQUE KEY `user_name` (`user_name`)
) ENGINE=InnoDB DEFAULT CHARSET=cp1251 AUTO_INCREMENT=26 ;

--
-- Dumping data for table `as_users`
--

INSERT INTO `as_users` VALUES(25, '2224', '222', '222', '2010-10-20 19:26:33', 0);

DELIMITER $$
--
-- Procedures
--
DROP PROCEDURE IF EXISTS `add_room`$$
CREATE DEFINER=`assoft_user`@`%` PROCEDURE `add_room`(_host_name varchar(255), _name varchar(255))
BEGIN
DECLARE _host_id int(63);
SELECT user_id INTO _host_id FROM as_users WHERE user_name  = _host_name LIMIT 1;  
INSERT INTO
    as_chat_room (
    room_id, 
    host_id, 
    host_name, 
    room_name, 
    tag)
  VALUES (NULL, _host_id, _host_name, _name, 0);
END$$

DROP PROCEDURE IF EXISTS `add_user`$$
CREATE DEFINER=`assoft_user`@`%` PROCEDURE `add_user`(name varchar(255), pass varchar(255), _ip varchar(16))
INSERT INTO
    as_users (
    user_id, 
    user_name, 
    password, 
    ip, 
    last_visit, 
    tag)
  VALUES (NULL, name, pass, _ip, CURRENT_TIMESTAMP, 0)$$

DROP PROCEDURE IF EXISTS `clean_users`$$
CREATE DEFINER=`assoft_user`@`%` PROCEDURE `clean_users`()
BEGIN
DELETE FROM as_users
        WHERE TIME_TO_SEC(TIMEDIFF(CURRENT_TIMESTAMP, last_visit)) > 60;
END$$

DROP PROCEDURE IF EXISTS `get_rooms_list`$$
CREATE DEFINER=`assoft_user`@`%` PROCEDURE `get_rooms_list`()
BEGIN
  SELECT host_name, room_name FROM as_chat_room;
END$$

DROP PROCEDURE IF EXISTS `get_users_list`$$
CREATE DEFINER=`assoft_user`@`%` PROCEDURE `get_users_list`()
BEGIN
  SELECT user_name, ip, last_visit, TIMEDIFF(CURRENT_TIMESTAMP, last_visit) FROM as_users;
END$$

DROP PROCEDURE IF EXISTS `update_last_visit`$$
CREATE DEFINER=`assoft_user`@`%` PROCEDURE `update_last_visit`(name VARCHAR(255))
BEGIN
  UPDATE as_users SET last_visit=CURRENT_TIMESTAMP WHERE user_name=name;
END$$

DELIMITER ;
