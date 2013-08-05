<?php
    //TODO:should check the user's login status, then do the tings below
    // the appid and appkey distributed by ecomet server
    $app_id = 1;
    $secret = "owlbiyqf";

    $user_id = $_GET['uid'];
    //the newest timestamp
    $timestamp = time();
    $plain = "$app_id$user_id$timestamp$secret";
    $sign = md5($plain);
    
    $result = array('sign'=>$sign,
                    'r'=>$timestamp
                    );

    echo json_encode($result);
?>
