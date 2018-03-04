package com.reallifedeveloper.uu_1dl251.infrastructure.persistence;

import org.springframework.data.jpa.repository.JpaRepository;

import com.reallifedeveloper.uu_1dl251.domain.User;
import com.reallifedeveloper.uu_1dl251.domain.UserRepository;

public interface JpaUserRepository extends UserRepository, JpaRepository<User, String> {

}
