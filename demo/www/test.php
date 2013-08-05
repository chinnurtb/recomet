<?php
    $secret = "owlbiyqf";
    $user_id = $_GET['user_id'];
    $app_id = 1;
    $timestamp = time();
    $plain = "$app_id$user_id$timestamp$secret";
    echo "$plain<br/>";
    $sign = md5($plain);
    echo $sign;  
?>
<html>
<head>
        <script src="js/jquery-2.0.2.min.js"></script>
</head>

<body>
    test longpoll
    <div id="content">
    </div>

    <script>
        retry_time = 1;
        token_request_data = {uid:"<?php echo $user_id;?>",
                       timestamp:"<?php echo $timestamp;?>",
                       sign:"<?php echo $sign;?>"
                      };

        function refresh_token(){
            $.ajax({
                type: "GET",
                url: "/authtoken.php",
                data: {uid:token_request_data.uid},
                cache:false,
                dataType: "json",
                success: function(ret){ 
                    if(ret){
                        token_request_data.timestamp = ret.r;
                        token_request_data.sign = ret.sign;
                        long_poll();
                    }
                }
            });
        }

        function long_poll() {
            $.ajax({
                type: "GET",
                url: "http://ecomet.etao.com:8080/longpoll/1?",
                data: token_request_data,
                cache:false,
                dataType: "json",
                success: function(ret){ 
                   if (ret.result == -2){
                        //token expired, should request new token
                        retry_time = retry_time * 2;
                        setTimeout(refresh_token, retry_time*1000);

                    } else if(ret[0].content){
                        retry_time = 1;
                        $("#content").append(ret[0].content);
                        long_poll();

                    } else {
                        //alert(ret);
                        //$("#content").append(ret.msg+"<br/>");
                        retry_time = retry_time * 2;
                        setTimeout(long_poll, retry_time*1000);

                    } 
                    
                },
                error : function() {
                    retry_time = retry_time * 2;
                    setTimeout(long_poll, retry_time*1000);
                }
            });
        }

        $(document).ready(function(){
            long_poll();
        });
    </script>
</body>
</html>
