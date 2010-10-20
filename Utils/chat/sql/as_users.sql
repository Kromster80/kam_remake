-- phpMyAdmin SQL Dump
-- version 3.3.7
-- http://www.phpmyadmin.net
--
-- Host: localhost
-- Generation Time: Oct 19, 2010 at 11:58 PM
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
-- Table structure for table `as_users`
--

CREATE TABLE IF NOT EXISTS `as_users` (
  `user_id` bigint(20) NOT NULL auto_increment,
  `user_name` varchar(255) NOT NULL,
  `password` varchar(255) NOT NULL,
  `ip` varchar(16) NOT NULL,
  `last_visit` timestamp NOT NULL default CURRENT_TIMESTAMP,
  `tag` int(11) NOT NULL default '0',
  PRIMARY KEY  (`user_id`),
  UNIQUE KEY `user_name` (`user_name`)
) ENGINE=InnoDB DEFAULT CHARSET=cp1251 AUTO_INCREMENT=3 ;

--
-- Dumping data for table `as_users`
--

INSERT INTO `as_users` (`user_id`, `user_name`, `password`, `ip`, `last_visit`, `tag`) VALUES
(1, 'Sasha', 'Sasha', '', '2010-10-19 23:21:59', 0),
(2, '111', '111', '', '2010-10-19 23:55:56', 0);

DELIMITER $$
--
-- Procedures
--
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

DELIMITER ;
