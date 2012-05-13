cmake_minimum_required( VERSION 2.6 )
find_package ( Qt4 REQUIRED )
include ( ${QT_USE_FILE} )
set ( CMAKE_BUILD_TYPE Release )

add_definitions ( -Wall -DUSE_LEVELDB -DBUILDING_RDM )

include_directories (
	${CMAKE_SOURCE_DIR} ${CMAKE_CURRENT_BINARY_DIR}
	${QT_QTCORE_INCLUDE_DIR} ${QT_QTNETWORK_INCLUDE_DIR}
	.
	../3rdparty/leveldb/src/include)

set ( rdm_HDRS
	Indexer.h
	SHA256.h
	Rdm.h
	IndexerJob.h
	Source.h
	DirtyJob.h
	Job.h
	CursorInfo.h
	AbortInterface.h
	MemoryMonitor.h
	Database.h
	Location.h
	)

set ( rdm_SRCS
	main.cpp
	Indexer.cpp
	Server.cpp
	SHA256.cpp
	DumpJob.cpp
	FollowLocationJob.cpp
	CursorInfoJob.cpp
	MatchJob.cpp
	ReferencesJob.cpp
	Rdm.cpp
	StatusJob.cpp
	IndexerJob.cpp
	DirtyJob.cpp
	Job.cpp
	TestJob.cpp
	MemoryMonitor.cpp
	Database.cpp
	Location.cpp
	)

set ( rdm_MOCS
	Server.h
	DumpJob.h
	FollowLocationJob.h
	CursorInfoJob.h
	MatchJob.h
	ReferencesJob.h
	StatusJob.h
	TestJob.h
        IndexerJob.h
        Job.h
        Indexer.h
	)

include ( ../shared/shared.cmake )
include ( ../shared/clang.cmake )
include ( ../shared/PCH_GCC4_v2.cmake )

QT4_WRAP_CPP( MOCS ${rdm_MOCS} ${shared_MOCS} )
QT4_GENERATE_MOCS ( ${shared_CPPMOCS} )

ADD_PCH_RULE ( Pch.h rdm_SRCS rdm_PCHFLAGS )
add_definitions( ${rdm_PCHFLAGS} )

add_executable ( rdm ${rdm_SRCS} ${shared_SRCS} ${UIS} ${RSCS} ${TRS} ${MOCS} )
target_link_libraries ( rdm ${QT_QTCORE_LIBRARY} ${QT_QTNETWORK_LIBRARY} ${clang_LIBS} )
