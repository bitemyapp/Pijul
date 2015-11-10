/*
  Copyright Florent Becker and Pierre-Etienne Meunier 2015.

  This file is part of Pijul.

  Pijul is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  Pijul is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with Pijul.  If not, see <http://www.gnu.org/licenses/>.
*/

#include <string.h>
#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/misc.h>
#include <caml/mlvalues.h>
#include <caml/signals.h>
#include <caml/unixsupport.h>
#include <lmdb.h>

#include <errno.h>

CAMLprim value caml_mdb_env_create(){
  CAMLparam0();
  MDB_env *env;
  if(mdb_env_create(&env)){
    caml_failwith("error in mdb_env_create");
  }
  CAMLreturn(env);
}
CAMLprim value caml_mdb_env_copy(value env,value path){
  CAMLparam2(env,path);
  if(mdb_env_copy((MDB_env*)env,String_val(path))){
    caml_failwith("error in mdb_env_copy");
  }
  CAMLreturn0;
}

CAMLprim value caml_mdb_env_open(value env,value path,value flags,value mode){
  CAMLparam4(env,path,flags,mode);
  if(mdb_env_open((MDB_env*)env,String_val(path),Int_val(flags),Int_val(mode))){
    caml_failwith("error in mdb_env_open");
  }
  CAMLreturn(env);
}

CAMLprim value caml_mdb_env_set_mapsize(value env,value size){
  CAMLparam2(env,size);
  if(mdb_env_set_mapsize((MDB_env*)env, Int_val(size))){
    caml_failwith("error in mdb_env_set_mapsize");
  }
  CAMLreturn0;
};

CAMLprim value caml_mdb_txn_begin(value env,value parent_,value flags){
  CAMLparam3(env,parent_,flags);
  MDB_txn*parent = (parent_!=Val_int(0)) ? ((MDB_txn*)Field(parent_,0)) : NULL;
  MDB_txn*txn;
  if(mdb_txn_begin((MDB_env*)env,parent,Int_val(flags),&txn)){
    caml_failwith("error in mdb_txn_begin");
  }
  CAMLreturn(txn);
}

CAMLprim value caml_mdb_txn_abort(value txn){
  CAMLparam1(txn);
  mdb_txn_abort((MDB_txn*) txn);
  CAMLreturn0;
}
CAMLprim value caml_mdb_txn_commit(value txn){
  CAMLparam1(txn);
  if(mdb_txn_commit((MDB_txn*) txn)){
    caml_failwith("error in mdb_txn_commit");
  }
  CAMLreturn0;
}


CAMLprim value caml_mdb_dbi_open(value txn,value name,value flags){
  CAMLparam3(txn,name,flags);
  MDB_dbi dbi;
  char*str=NULL;
  if(caml_string_length(name))
    str=String_val(name);

  if(mdb_dbi_open((MDB_txn*)txn,str,Int_val(flags),&dbi)){
    caml_failwith("error in mdb_dbi_open");
  }

  CAMLreturn(Val_int(dbi));
}


CAMLprim value caml_mdb_put(value txn,value dbi,value key,value data,value flags){
  CAMLparam5(txn,dbi,key,data,flags);
  MDB_val key_,data_;
  key_.mv_data=String_val(key);
  key_.mv_size=caml_string_length(key);
  data_.mv_data=String_val(data);
  data_.mv_size=caml_string_length(data);
  if(mdb_put((MDB_txn*)txn,
             (MDB_dbi) Int_val(dbi),
             &key_,
             &data_,
             Int_val(flags)
             )){
    caml_failwith("error in mdb_put");
  }
  CAMLreturn0;
}

CAMLprim value caml_mdb_get(value txn,value dbi,value key){
  CAMLparam3(txn,dbi,key);
  CAMLlocal1(mldata);
  MDB_val key_,data_;
  key_.mv_data=String_val(key);
  key_.mv_size=caml_string_length(key);
  int ret;
  if((ret=mdb_get(  (MDB_txn*)txn,  (MDB_dbi) Int_val(dbi),  &key_,  &data_  ))){
    if(ret==MDB_NOTFOUND) {
      static value *exn=NULL;
      if(exn==NULL) exn=caml_named_value("lmdb_not_found");
      caml_raise_constant(*exn);
    } else
      caml_failwith("error in mdb_get");
  }
  mldata=caml_alloc_string(data_.mv_size);
  memcpy(String_val(mldata),data_.mv_data,data_.mv_size);
  CAMLreturn(mldata);
}


CAMLprim value caml_mdb_del(value txn,value dbi,value key,value data){
  CAMLparam4(txn,dbi,key,data);
  MDB_val key_,data_;
  key_.mv_data=String_val(key);
  key_.mv_size=caml_string_length(key);
  int ret;
  if(data ==Val_int(0)){
    if((ret=mdb_del(  (MDB_txn*)txn,  (MDB_dbi) Int_val(dbi),  &key_, NULL ))){
      if(ret==MDB_NOTFOUND) {
        static value *exn=NULL;
        if(exn==NULL) exn=caml_named_value("lmdb_not_found");
        caml_raise_constant(*exn);
      } else
        caml_failwith("error in mdb_del");
    }
  } else {
    value x=Field(data,0);
    data_.mv_data=String_val(x);
    data_.mv_size=caml_string_length(x);
    if((ret=mdb_del(  (MDB_txn*)txn,  (MDB_dbi) Int_val(dbi),  &key_, &data_ ))){
      caml_failwith("error in mdb_del");
    }
  }

  CAMLreturn0;
}



CAMLprim value caml_mdb_dbi_close(value env,value dbi){
  CAMLparam2(env,dbi);
  mdb_dbi_close((MDB_env*)env,(MDB_dbi) Int_val(dbi));
  CAMLreturn0;
}
CAMLprim value caml_mdb_drop(value txn,value dbi,value del){
  CAMLparam3(txn,dbi,del);
  if(mdb_drop((MDB_txn*)txn,(MDB_dbi) Int_val(dbi),Bool_val(del))){
    caml_failwith("error in mdb_drop");
  }
  CAMLreturn0;
}
CAMLprim value caml_mdb_env_close(value env){
  CAMLparam1(env);
  mdb_env_close((MDB_env*) env);
  CAMLreturn0;
}
CAMLprim value caml_mdb_reader_check(value env){
  CAMLparam1(env);
  int dead;
  if((mdb_reader_check( (MDB_env*)env, &dead ))){
    caml_failwith("error in mdb_reader_check");
  }
  CAMLreturn(Val_int(dead));
}
CAMLprim value caml_mdb_env_set_maxdbs(value env,value m){
  CAMLparam2(env,m);
  if(mdb_env_set_maxdbs((MDB_env*)env,Int_val(m))){
    caml_failwith("error in mdb_env_set_maxdbs");
  }
  CAMLreturn0;
}


CAMLprim value caml_mdb_cursor_open(value txn,value dbi){
  CAMLparam2(txn,dbi);
  MDB_cursor *curs;
  if(mdb_cursor_open( (MDB_txn*) txn,  (MDB_dbi) Int_val(dbi),  &curs)){
    caml_failwith("error in mdb_cursor_open");
  }
  CAMLreturn((value)curs);
}
CAMLprim value caml_mdb_cursor_renew(value txn,value cursor){
  CAMLparam2(txn,cursor);
  if(mdb_cursor_renew( (MDB_txn*) txn,  (MDB_cursor*) cursor)){
    caml_failwith("error in mdb_cursor_renew");
  }
  CAMLreturn0;
}
CAMLprim value caml_mdb_cursor_close(value curs){
  CAMLparam1(curs);
  mdb_cursor_close( (MDB_cursor*) curs );
  CAMLreturn0;
}

CAMLprim value caml_mdb_cursor_get(value curs,value key,value data,value op){
  CAMLparam4(curs,key,data,op);
  CAMLlocal3(result,mlkey,mldata);
  MDB_val key_,data_;
  key_.mv_data=String_val(key);
  key_.mv_size=caml_string_length(key);
  data_.mv_data=String_val(data);
  data_.mv_size=caml_string_length(data);

  int ret;
  if((ret=mdb_cursor_get(  (MDB_cursor*)curs,  &key_,  &data_, Int_val(op) ))){
    if(ret==MDB_NOTFOUND) {
      static value *exn=NULL;
      if(exn==NULL) exn=caml_named_value("lmdb_not_found");
      caml_raise_constant(*exn);
    } else
      caml_failwith("error in mdb_cursor_get");
  }
  mlkey=caml_alloc_string(key_.mv_size);
  memcpy(String_val(mlkey),key_.mv_data,key_.mv_size);
  mldata=caml_alloc_string(data_.mv_size);
  memcpy(String_val(mldata),data_.mv_data,data_.mv_size);
  result=caml_alloc(2,0);
  Store_field(result,0,mlkey);
  Store_field(result,1,mldata);
  CAMLreturn(result);
}


CAMLprim value caml_mdb_cursor_del(value cursor,value flags){
  CAMLparam2(cursor,flags);
  if(mdb_cursor_del((MDB_cursor*)cursor, Int_val(flags))){
    caml_failwith("error in mdb_cursor_del");
  }
  CAMLreturn0;
}

CAMLprim value caml_mdb_cursor_put(value cursor,value key,value data,value flags){
  CAMLparam4(cursor,flags,key,data);
  MDB_val key_,data_;
  key_.mv_data=String_val(key);
  key_.mv_size=caml_string_length(key);
  data_.mv_data=String_val(data);
  data_.mv_size=caml_string_length(data);
  if(mdb_cursor_put((MDB_cursor*)cursor, &key_,&data_,Int_val(flags))){
    caml_failwith("error in mdb_cursor_put");
  }
  CAMLreturn0;
}

CAMLprim value caml_mdb_cursor_count(value cursor){
  CAMLparam1(cursor);
  size_t count;
  if(mdb_cursor_count((MDB_cursor*)cursor, &count)){
    caml_failwith("error in mdb_cursor_count");
  }
  CAMLreturn(Val_int(count));
}


/////////////// Other stubs


CAMLprim value caml_cursor_position(value curs,value key,value data,value op){
  CAMLparam4(curs,key,data,op);
  MDB_val key_,data_;
  key_.mv_data=String_val(key);
  key_.mv_size=caml_string_length(key);
  data_.mv_data=String_val(data);
  data_.mv_size=caml_string_length(data);

  int ret=mdb_cursor_get(  (MDB_cursor*)curs,  &key_,  &data_, Int_val(op) );

  CAMLreturn(Val_int(ret));
}

int has_neighbors(MDB_cursor* curs, MDB_val* key_, char flag){
  MDB_val data_;
  data_.mv_data= &flag;
  data_.mv_size= 1;
  int ret=mdb_cursor_get( (MDB_cursor*)curs, key_, &data_, MDB_GET_BOTH_RANGE );
  if(!ret && ((char*)data_.mv_data)[0] == flag){
    return 1;
  } else {
    return 0;
  }
}

CAMLprim value caml_has_neighbors(value curs,value key,value flag){
  CAMLparam3(curs,key,flag);
  MDB_val key_;
  char c=Int_val(flag);
  key_.mv_data=String_val(key);
  key_.mv_size=caml_string_length(key);
  if(has_neighbors((MDB_cursor*) curs, &key_, c))
    CAMLreturn(Val_true);
  else
    CAMLreturn(Val_false);
}

CAMLprim value caml_delete_edges(value curs,value a_,value flag){
  CAMLparam3(curs,a_,flag);
  MDB_val a,b;
  int ret=0;
  char c=(char) Int_val(flag);
  char aa[24];
  char bb[45];
  char*pb;
  while(1){
    a.mv_data=String_val(a_);
    a.mv_size=caml_string_length(a_);
    b.mv_data=&c;
    b.mv_size=1;
    ret=mdb_cursor_get((MDB_cursor*)curs,&a,&b,MDB_GET_BOTH_RANGE);
    pb=(char*)b.mv_data;
    if(ret || pb[0]!=c) break;
    // We could delete here, but then the memory that a and b point to could be changed by cursor_del.
    memcpy(aa,pb+1,24);
    a.mv_data=aa;
    a.mv_size=24;

    bb[0]=(pb[0]) ^ 4;
    memcpy(bb+1,String_val(a_),24);
    memcpy(bb+25,pb+25,20);

    b.mv_data=bb;
    b.mv_size=45;

    if((ret=mdb_cursor_del((MDB_cursor*)curs,0))) break;
    if((ret=mdb_cursor_get((MDB_cursor*)curs,&a,&b,MDB_GET_BOTH))) break;
    if((ret=mdb_cursor_del((MDB_cursor*)curs,0))) break;
  }
  CAMLreturn0;
}
/*
#define DELETED_FLAG 8
#define PARENT_FLAG 4
#define FOLDER_FLAG 2
#define PSEUDO_FLAG 1

struct buffer{
  char*buf;
  int pos;
  int size;
};
#define KEY_SIZE 24
void connect_plus(MDB_txn *txn, MDB_dbi dbi, MDB_cursor *curs, MDB_val* a, struct buffer*buf){

  if(has_neighbors(curs,a,PARENT_FLAG)
     || has_neighbors(curs,a,PARENT_FLAG | PSEUDO_FLAG)
     || has_neighbors(curs,a,PARENT_FLAG | FOLDER_FLAG)
     || has_neighbors(curs,a,PARENT_FLAG | PSEUDO_FLAG | FOLDER_FLAG)) {
    if(buf->pos + KEY_SIZE > buf->size){
      buf->size*=2;
      buf->buf=realloc(buf->buf,buf->size);
    }
    memcpy(buf->buf + buf->pos, a->mv_data, KEY_SIZE);
    buf->pos+=KEY_SIZE;
  }
  printf("a=");
  int j;
  for(j=0;j<KEY_SIZE;j++) {
    unsigned char x=((char*)a->mv_data)[j];
    printf("%x",x & 0xff);
  }
  printf("\n");
  
  MDB_val b,c;
  char flag=PARENT_FLAG | DELETED_FLAG;
  b.mv_data=&flag;
  b.mv_size=1;
  MDB_cursor *tmp;
  if(mdb_cursor_open(txn, dbi,&tmp)) return;
  int ret=mdb_cursor_get((MDB_cursor*)tmp,a,&b,MDB_GET_BOTH_RANGE);
  if(ret) return;
  while(1){
    c.mv_data=(((char*)b.mv_data)+1);
    c.mv_size=24;

    connect_plus(txn,dbi,curs,&c,buf);

    ret=mdb_cursor_get(tmp,a,&b,MDB_NEXT_DUP);
    if(ret || ((char*)b.mv_data)[0] != flag) break;
  }
  mdb_cursor_close((MDB_cursor*) tmp);
}


CAMLprim value caml_connect_plus(value txn,value dbi,value curs,value a_,value b_){
  CAMLparam5(txn,dbi,curs,a_,b_);
  MDB_val a,b;
  a.mv_data=String_val(a_);
  a.mv_size=caml_string_length(a_);
  struct buffer buffer;
  buffer.size=240; // 10 keys
  buffer.buf=malloc(buffer.size);
  buffer.pos=0;
  connect_plus((MDB_txn*)txn,(MDB_dbi)dbi,(MDB_cursor*)curs,&a,&buffer);
  int i;
  char bb[45];
  memset(bb+25,0,20);
  for(i=0;i<buffer.pos;i+=KEY_SIZE){
    a.mv_size=24;
    a.mv_data=buffer.buf + i;
    b.mv_size=45;
    b.mv_data=bb;
    bb[0]=PSEUDO_FLAG;
    memcpy(bb+1,String_val(b_),KEY_SIZE);
    int j;
    printf("app: ");
    for(j=0;j<KEY_SIZE;j++)
      printf(".%x",0xff & buffer.buf[i+j]);
    printf("\n");
    for(j=0;j<KEY_SIZE;j++)
      printf(".%x",0xff & bb[1+j]);
    printf("\n/app\n");
    mdb_put((MDB_txn*)txn,(MDB_dbi)dbi,&a,&b,0);

    bb[0]=PSEUDO_FLAG | PARENT_FLAG;
    memcpy(bb+1,buffer.buf+i,KEY_SIZE);
    a.mv_data=String_val(b_);
    mdb_put((MDB_txn*)txn,(MDB_dbi)dbi,&a,&b,0);
  }
  free(buffer.buf);
  CAMLreturn0;
}
*/
