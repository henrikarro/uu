package com.reallifedeveloper.uu_1dl251.domain;

public interface UserRepository {

	User findByUserId(String userId);

	<U extends User> U save(U user);

}
