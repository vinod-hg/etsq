#include "etsq.h"

ErlNifRWLock *qinfo_rwlock;
QInfoMap qinfo_map;

void new_q(char* name)
{
	std::cout<<"Create: " << name<<std::endl;
	WriteLock write_lock(qinfo_rwlock);
	QueueInfo queue_info;
	qinfo_map.insert(QInfoMapPair(name, queue_info));
	std::cout<<"Created: " << name<<std::endl;
}

int get_q_info(char* name, QueueInfo* pqueue_info)
{
	std::cout<<"Info: "<< name<<std::endl;
	QInfoMap::iterator iter = qinfo_map.find(name);
	if (iter != qinfo_map.end())
	{
		*pqueue_info = iter->second;
		std::cout<<" Fetched ";
		return 1;
	}
	return 0;
}

//int push()
//{
//	return -1;
//}
//
//int pop()
//{
//	return -1;
//}

static ERL_NIF_TERM new_queue(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	int size = 100;
	char *name = new char(size);
	enif_get_atom(env, argv[0], name, size, ERL_NIF_LATIN1);
	new_q(name);
	return enif_make_int(env, 0);
}

static ERL_NIF_TERM info(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	int size = 100;
	char name[size];
	enif_get_atom(env, argv[0], name, size, ERL_NIF_LATIN1);
	QueueInfo pqueue_info;
	int return_code = 0;
	{
		ReadLock rlock(qinfo_rwlock);
		return_code = get_q_info(name, &pqueue_info);
	}
	return enif_make_int(env, return_code);
}


static ERL_NIF_TERM push(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	int size = 100;
	char name[size];
	enif_get_atom(env, argv[0], name, size, ERL_NIF_LATIN1);
	ErlNifBinary* test = new ErlNifBinary;
	ErlNifBinary* bin = new ErlNifBinary;
	int return_code = 0;
	if( true == enif_inspect_binary(env, argv[1], test))
	{
		std::cout<< test->size <<" ";
		if( 1 == enif_alloc_binary(bin->size, bin))
		{
			std::cout<< bin <<" ";
			QueueInfo pqueue_info;
			{
				ReadLock rlock(qinfo_rwlock);
				if (1 == get_q_info(name, &pqueue_info))
				{
					return_code = 1;
					pqueue_info.queue.push(bin);
					std::cout<<" Sucessfully pushed ";
				}

			}
		}
	}
	return enif_make_int(env, return_code);
}

static ERL_NIF_TERM pop(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	int size = 100;
	char name[size];
	enif_get_atom(env, argv[0], name, size, ERL_NIF_LATIN1);
	QueueInfo pqueue_info;
	int return_code = -1;
	ErlNifBinary *bin = NULL;
	std::cout<< " pop called "<< name;
	{
		ReadLock rlock(qinfo_rwlock);
		if (1 == get_q_info(name, &pqueue_info))
		{
		std::cout<<" success ";
		std::cout << ' ' << pqueue_info.queue.front();
//		bin = pqueue_info.queue.front();
//
//		std::cout<< bin->size <<" ";
//		std::cout<< bin <<" ";
//		return enif_make_binary(env, bin);
		}
	}
	std::cout<< " error ";
	return enif_make_int(env, return_code);
}

static int is_ok_load_info(ErlNifEnv* env, ERL_NIF_TERM load_info)
{
    int i;
    return enif_get_int(env, load_info, &i) && i == 1;
}

static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    if (!is_ok_load_info(env, load_info))
    	return -1;
    qinfo_rwlock = enif_rwlock_create((char*)"qinfo");
    return 0;
}

static int upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data, ERL_NIF_TERM load_info)
{
    if (!is_ok_load_info(env, load_info))
	return -1;
    return 0;
}

static void unload(ErlNifEnv* env, void* priv_data)
{
	enif_rwlock_destroy(qinfo_rwlock);
}


static ErlNifFunc nif_funcs[] =  {
    {"new", 1, new_queue},
	{"info", 1, info},
	{"push_back", 2, push},
	{"pop_front", 1, pop}
};

ERL_NIF_INIT(etsq, nif_funcs, load, NULL, upgrade, unload)
