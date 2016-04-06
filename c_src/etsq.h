/*
 * etsq.h
 *
 *  Created on: Mar 21, 2016
 *      Author: vinod
 */

#ifndef ETSQ_H_
#define ETSQ_H_

#include <iostream>       // std::cin, std::cout
#include <map>          // std::queue
#include <queue>          // std::queue
#include "erl_nif.h"

struct cmp_str
{
   bool operator()(char *a, char *b) const
   {
      return std::strcmp(a, b) < 0;
   }
};

typedef std::queue<ErlNifBinary*> ErlQueue;

typedef struct
{
	ErlNifMutex* pqueue_mtx;
	ErlQueue queue;
}QueueInfo;

typedef std::map<char *, QueueInfo, cmp_str> QInfoMap;
typedef std::pair<char *, QueueInfo> QInfoMapPair;

class ReadLock
{
	ErlNifRWLock *pread_lock;
public:
	ReadLock(ErlNifRWLock *pread_lock)
	{
		this->pread_lock = pread_lock;
		enif_rwlock_rlock(this->pread_lock);
	};
	~ReadLock()
	{
		enif_rwlock_runlock(pread_lock);
	};
};

class WriteLock
{
	ErlNifRWLock *pwrite_lock;
public:
	WriteLock(ErlNifRWLock *pwrite_lock)
	{
		this->pwrite_lock = pwrite_lock;
		enif_rwlock_rwlock(this->pwrite_lock);
	};
	~WriteLock()
	{
		enif_rwlock_rwunlock(pwrite_lock);
	};
};

class Mutex
{
	ErlNifMutex *pmtx;
public:
	Mutex(ErlNifMutex *pmtx)
	{
		this->pmtx = pmtx;
		enif_mutex_lock(this->pmtx);
	};
	~Mutex()
	{
		enif_mutex_unlock(pmtx);
	};
};

#endif /* ETSQ_H_ */
