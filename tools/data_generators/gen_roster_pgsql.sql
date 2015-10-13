CREATE OR REPLACE Function GenRoster(IN usersN INTEGER, IN rosterN INTEGER,
                        IN userN VARCHAR(255), IN hostB VARCHAR(255)) RETURNS void AS $$
	DECLARE
		x INT;
                y INT;
                z INTEGER;
                userA VARCHAR(255);
                userB VARCHAR(255);
        BEGIN
                x := 1;
                WHILE x < usersN LOOP
                        y := 0;
                        WHILE y < rosterN LOOP
                                userA := CONCAT(userN, y+x);
                                z := rosterN-1;
                                WHILE z >= 0 LOOP
                                        userB := CONCAT(userN, z+x);
                                        INSERT INTO rosterusers (username, jid, nick, subscription, ask, askmessage, server, type, created_at)
                                                VALUES (userA, CONCAT(userB, '@', hostB), userB, 'B','N', '', 'N','item',now());
                                        z := z - 1;
                                END LOOP;
                                y := y+1;
                        END LOOP;
                        x := x + rosterN;
                END LOOP;
        END;
        $$ LANGUAGE plpgsql;

-- usage
-- SELECT GenRoster(10, 10, 'user_', 'localhost');