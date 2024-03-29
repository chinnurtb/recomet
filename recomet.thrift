namespace cpp com.etao.recomet
namespace java com.etao.recomet
namespace php  com.etao.recomet

struct Message {
       1: i32 channel,
       2: i64 from,
       3: i64 to,
       4: string nick="",
       5: string type = "msg",
       6: string content,
       7: i32 created=0,
       8: bool offline=false,
       9: i32 expire=0
}


service RecometRouter {
    oneway void send(1:Message Msg),
    oneway void sends(1:i32 Appid, 2:i64 To, 3:list<Message> Msg, 4:bool Offline=false),
    i64 get_online_count(1:i32 AppId),
    list<i64> get_online_ids(1:i32 AppId)
}
