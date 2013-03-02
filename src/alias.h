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

#ifndef X64ASM_SRC_ALIAS_H
#define X64ASM_SRC_ALIAS_H

#include <cassert>
#include <type_traits>

#include "src/constants.h"
#include "src/mm.h"
#include "src/r.h"
#include "src/st.h"
#include "src/type_traits.h"
#include "src/xmm.h"
#include "src/ymm.h"

namespace x64asm {

class Alias {
	public:
		template <typename T>
			static typename std::enable_if<is_reg<T>::value && !std::is_same<T,Rh>::value && !std::is_same<T,Rb>::value, const Rl&>::type
			to_low(const T& t) {
				assert(r < 4);
				return rls[t];		
			}

		static const Rl& to_low(const Rh& r) {
			return rls[r-4];
		}

		template <typename T>
			static typename std::enable_if<is_reg<T>::value && !std::is_same<T,Rh>::value && !std::is_same<T,Rb>::value, const Rh&>::type
			to_high(const T& t) {
				assert(r < 4);
				return rhs[t+4];		
			}

		static const Rh& to_high(const Rh& r) {
			return r;
		}

		template <typename T>
			static typename std::enable_if<is_reg<T>::value && !std::is_same<T,Rl>::value && !std::is_same<T,Rh>::value, const Rb&>::type
			to_byte(const T& t) {
				assert(r >= 4);
				return rbs[t-4];		
			}

		template <typename T>
			static typename std::enable_if<is_reg<T>::value && !std::is_same<T,Rh>::value, const R16&>::type
			to_word(const T& t) {
				return r16s[t];
			}

		static const R16& to_word(const Rh& r) {
			return r16s[r-4];
		}

		template <typename T>
			static typename std::enable_if<is_reg<T>::value && !std::is_same<T,Rh>::value, const R32&>::type
			to_double(const T& t) {
				return r32s[t];
			}

		static const R32& to_double(const Rh& r) {
			return r32s[r-4];
		}

		template <typename T>
			static typename std::enable_if<is_reg<T>::value && !std::is_same<T,Rh>::value, const R64&>::type
			to_quad(const T& t) {
				return r64s[t];
			}

		static const R64& to_quad(const Rh& r) {
			return r64s[r-4];
		}

		static const Mm& to_mm(const St& st, const St& top) {
			return mms[(st+top) % 8];
		}

		static const St& to_st(const Mm& mm, const St& top) {
			return sts[(mm+top) % 8];
		}

		static const Xmm& to_xmm(const Ymm& y) {
			return xmms[y];
		}

		static const Ymm& to_ymm(const Xmm& x) {
			return ymms[x];
		}
};

} // namespace x64asm

#endif

