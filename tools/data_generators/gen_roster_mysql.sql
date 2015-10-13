DELIMITER $$
DROP PROCEDURE IF EXISTS GenRoster$$
CREATE PROCEDURE GenRoster(IN usersN INTEGER, IN rosterN INTEGER,
                        IN userN VARCHAR(255), IN hostB VARCHAR(255))
        BEGIN
                DECLARE x,y,z INTEGER;
                DECLARE userA, userB VARCHAR(255);
                SET x = 1;
                WHILE x < usersN DO
                        SET y = 0;
                        WHILE y < rosterN DO
                                SET userA = CONCAT(userN, y+x);
                                SET z = rosterN-1;
                                WHILE z >= 0 DO
                                        SET userB = CONCAT(userN, z+x);
                                        INSERT INTO rosterusers (username, jid, nick, subscription, ask, server, type, created_at)
                                                VALUES (userA, CONCAT(userB, '@', hostB), userB, 'B','N','N','item',now());
                                        SET z = z - 1;
                                END WHILE;
                                SET y = y+1;
                        END WHILE;
                        SET x = x + rosterN;
                END WHILE;
        END $$
