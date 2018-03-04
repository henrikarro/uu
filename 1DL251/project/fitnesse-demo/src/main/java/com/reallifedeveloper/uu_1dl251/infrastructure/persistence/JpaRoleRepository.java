package com.reallifedeveloper.uu_1dl251.infrastructure.persistence;

import org.springframework.data.jpa.repository.JpaRepository;

import com.reallifedeveloper.uu_1dl251.domain.Role;
import com.reallifedeveloper.uu_1dl251.domain.RoleRepository;

public interface JpaRoleRepository extends RoleRepository, JpaRepository<Role, String> {

}
