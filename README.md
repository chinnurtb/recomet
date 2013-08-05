recomet: A http longpolling application with riak_core
======================================
 
 TODO
  1、fix log 
  2、modulize



Authentication

1. Signature check
 
      First we should generate a appid-secret pair for user, 
            
            run start-web.sh
            auth_util:generate_key()

            (n1@127.0.0.1)1> auth_util:generate_key().
            10:43:40.928 [info] MaxKey is 0 
            10:43:40.930 [info] {auth_key,1,"owlbiyqf"} 
            {atomic,{auth_key,1,"owlbiyqf"}}


            then we got the appid = 1, and secret = owlbiyqf

            next we should change the appid and secret in demo/www/test.php

            <?php
            $secret = "owlbiyqf";
            $user_id = 1;
            $app_id = 1;
            ........


        Open the browser to url http://demo/test.php, hold on this page, this page will longpoll from the ecomet server on appid=1 with uid=1, and then open another browser on url http://demo:8080/send/1?content=test_content&uid=1, this api will send the message to user whose uid=1 with appid = 1
        
        If everything is ok, you will see the test_content will on the http://demo/test.php page you have opened before.

2. Ip Rules

        For such url as below
        http://demo:8080/send/1?content=test_content&uid=1
        
        we can set a ip access rule in 

        apps/web/priv/iprules.conf and target/ecomet/lib/ecomet/priv/iprules.conf
            
            when we run ecomet by start-web.sh, we should use apps/web/priv/iprules.conf

            when we run ecoment by target/ecomet/bin/ecomet start, we should use target/ecomet/lib/ecomet/priv/iprules.conf

            the iprules.conf like below

            [
            {
            iprules,[{allowed,["0.0.0.0/0"]},{denied,["10.1.26.49/32"]}]
            }
            ].


            when we changed the iprules, we can use auth_util:reload_ip_rules() to reload the config at runtime without restart the service. This can be done in start-web.sh shell or target/ecomet/bin/ecomet attach after you run the ecoment in application mode.
 
