#include "etsq.h"

ErlNifRWLock *qinfo_map_rwlock;
QInfoMap qinfo_map;

// Function finds the queue from map and returns QueueInfo
// Not thread safe.
QueueInfo* get_q_info(char* name)
{
	//std::cout<<"Info: "<< name<<std::endl;
	QInfoMap::iterator iter = qinfo_map.find(name);
	if (iter != qinfo_map.end())
	{
		//std::cout<<" Fetched ";
		return iter->second;
	}
	return NULL;
}

void new_q(char* name)
{
	//std::cout<<"Create: " << name<<std::endl;
	WriteLock write_lock(qinfo_map_rwlock);
	QueueInfo *queue_info = new QueueInfo(name);
	qinfo_map.insert(QInfoMapPair(name, queue_info));
	//std::cout<<"Created: " << name<<std::endl;
}

bool push(char* name, ErlTerm *erl_term)
{
	QueueInfo *pqueue_info = NULL;
	ReadLock read_lock(qinfo_map_rwlock);
	if (NULL != (pqueue_info = get_q_info(name)))
	{
		Mutex mutex(pqueue_info->pmutex);
		pqueue_info->queue.push(erl_term);
		return true;
	}
	return false;
}

// Returns new ErlTerm. Caller should delete it
ErlTerm* pop(char* name, bool read_only)
{
	QueueInfo *pqueue_info = NULL;
	ReadLock read_lock(qinfo_map_rwlock);
	if (NULL != (pqueue_info = get_q_info(name)))
	{
		Mutex mutex(pqueue_info->pmutex);
		if (!pqueue_info->queue.empty())
		{
			ErlTerm *erl_term = pqueue_info->queue.front();
			if(read_only)
			{
				return new ErlTerm(erl_term);
			}
			pqueue_info->queue.pop();
			return erl_term;
		}
		return new ErlTerm("empty");
	}
	return NULL;
}

static ERL_NIF_TERM new_queue(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	int size = 100;
	char *name = new char(size);
	enif_get_atom(env, argv[0], name, size, ERL_NIF_LATIN1);
	{
		QueueInfo *pqueue_info = NULL;
		ReadLock read_lock(qinfo_map_rwlock);
		if (NULL != (pqueue_info = get_q_info(name)))
		{
			return enif_make_error(env, "already_exists");
		}
	}
	new_q(name);
	return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM info(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	int size = 100;
	char name[size];
	enif_get_atom(env, argv[0], name, size, ERL_NIF_LATIN1);
	int queue_size = 0;
	{
		QueueInfo *pqueue_info = NULL;
		ReadLock read_lock(qinfo_map_rwlock);
		if (NULL == (pqueue_info = get_q_info(name)))
			return enif_make_badarg(env);
		queue_size = pqueue_info->queue.size();
	}
	return enif_make_list2(env,
			enif_make_tuple2(env, enif_make_atom(env, "name"), enif_make_atom(env, name)),
			enif_make_tuple2(env, enif_make_atom(env, "size"), enif_make_int(env, queue_size)));
}

static ERL_NIF_TERM push_back(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	int size = 100;
	char name[size];
	enif_get_atom(env, argv[0], name, size, ERL_NIF_LATIN1);
	ErlTerm *erl_term = new ErlTerm(argv[1]);
	if (push(name, erl_term))
		return enif_make_atom(env, "ok");
	delete erl_term;
	return enif_make_badarg(env);
}

static ERL_NIF_TERM pop_front(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	int size = 100;
	char name[size];
	enif_get_atom(env, argv[0], name, size, ERL_NIF_LATIN1);
	ErlTerm *erl_term = NULL;
	if (NULL == (erl_term = pop(name, false)))
		return enif_make_badarg(env);
	ERL_NIF_TERM return_term = enif_make_copy(env, erl_term->term);
	delete erl_term;
	return return_term;
}

static ERL_NIF_TERM get_front(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	int size = 100;
	char name[size];
	enif_get_atom(env, argv[0], name, size, ERL_NIF_LATIN1);
	ErlTerm *erl_term = NULL;
	if (NULL == (erl_term = pop(name, true)))
		return enif_make_badarg(env);
	ERL_NIF_TERM return_term = enif_make_copy(env, erl_term->term);
	delete erl_term;
	return return_term;
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
    qinfo_map_rwlock = enif_rwlock_create((char*)"qinfo");
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
	enif_rwlock_destroy(qinfo_map_rwlock);
}

static ErlNifFunc nif_funcs[] =  {
    {"new", 1, new_queue},
	{"info", 1, info},
	{"push_back", 2, push_back},
	{"pop_front", 1, pop_front},
	{"get_front", 1, get_front}
};

ERL_NIF_INIT(etsq, nif_funcs, load, NULL, upgrade, unload)
