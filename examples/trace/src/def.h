/*
Copyright 2103 eric schkufza

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*/

#ifdef CPU_STATE_AVX
	#define SSE_SIZE 4
	#define SSE_TYPE Ymm
	#define SSE_POOL ymms
	#define SSE_MOV vmovdqu
	#define SSE_MEM M256
#else
	#define SSE_SIZE 2
	#define SSE_TYPE Xmm
	#define SSE_POOL xmms
	#define SSE_MOV movdqu
	#define SSE_MEM M128
#endif

#define BYTES_PER_SSE (8*SSE_SIZE)
#define WORDS_PER_SSE (4*SSE_SIZE)
#define DOUBLES_PER_SSE (2*SSE_SIZE)
#define QUADS_PER_SSE (1*SSE_SIZE)
